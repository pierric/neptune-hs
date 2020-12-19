{-|
Module      : Neptune.OAuth
Description : Neptune Client
Copyright   : (c) Jiasen Wu, 2020
License     : BSD-3-Clause
-}
{-# LANGUAGE TemplateHaskell #-}
module Neptune.OAuth where

import           Control.Concurrent    (ThreadId, forkIO)
import           Control.Lens
import qualified Data.Aeson            as Aeson
import           Data.Aeson.Lens
import           Data.Time.Clock       (NominalDiffTime)
import           Data.Time.Clock.POSIX (getPOSIXTime)
import           Network.HTTP.Req      (JsonResponse, POST (..),
                                        ReqBodyUrlEnc (..), defaultHttpConfig,
                                        jsonResponse, req, responseBody, runReq,
                                        useHttpsURI, (=:))
import           RIO                   hiding (Lens', (^.))
import qualified RIO.Text              as T
import           Text.URI              (mkURI)
import qualified Web.JWT               as JWT


data OAuth2Session = OAuth2Session
    { _oas_client_id     :: Text
    , _oas_access_token  :: Text -- ^ access token for APIs
    , _oas_refresh_token :: Text -- ^ refresh token after expires
    , _oas_expires_in    :: NominalDiffTime -- ^ duration in which the access token is valid
    , _oas_refresh_url   :: Text -- ^ refresh url
    }

makeLenses ''OAuth2Session

-- | Setup a background thread to refresh OAuth2 token.
oauth2Setup :: Text -> Text -> IO (ThreadId, MVar OAuth2Session)
oauth2Setup access_token refresh_token = do
    let decoded = JWT.decode $ access_token
        claims  = JWT.claims (decoded ^?! _Just)
        issuer  = JWT.iss claims ^?! _Just & JWT.stringOrURIToText
        refresh_url = T.append issuer "/protocol/openid-connect/token"
        client_name = JWT.unClaimsMap (JWT.unregisteredClaims claims) ^?! ix "azp" . _String
        expires_at  = JWT.exp claims ^?! _Just & JWT.secondsSinceEpoch

    now <- getPOSIXTime
    let expires_in = expires_at - now
        session = OAuth2Session client_name access_token refresh_token expires_in refresh_url

    oauth_session_var <- newMVar session
    refresh_thread <- forkIO (oauthRefresher oauth_session_var)
    return (refresh_thread, oauth_session_var)

oauthRefresher :: MVar OAuth2Session -> IO ()
oauthRefresher session_var = update >> oauthRefresher session_var
    where
        update = do
            session <- readMVar session_var
            let wait_sec = floor $ 1000000 * session ^. oas_expires_in
            threadDelay wait_sec

            modifyMVar_ session_var $ \session -> do
                let url = session ^. oas_refresh_url
                    tok = session ^. oas_refresh_token
                    body = ReqBodyUrlEnc $ mconcat
                            [ "grant_type"    =: ("refresh_token" :: Text)
                            , "refresh_token" =: tok
                            , "client_id"     =: ("neptune-cli"   :: Text) ]

                {--
                Accept: application/json
                Content-Type: application/x-www-form-urlencoded;charset=UTF-8

                grant_type=refresh_token
                &refresh_token=...
                &client_id=neptune-cli
                --}
                case useHttpsURI =<< mkURI url of
                  Nothing -> error $ "Bad refresh url " ++ T.unpack url
                  Just (url, opt) -> do
                      -- the default retrying pocily is 50ms delay, upto 5 times
                      resp <- runReq defaultHttpConfig $ req POST url body jsonResponse opt :: IO (JsonResponse Aeson.Value)
                      resp <- pure $ responseBody resp
                      -- resp is a json object of keys:
                      --    "access_token", "expires_in", "refresh_expires_in", "refresh_token",
                      --    "token_type", "not-before-policy", "session_state", "scope"
                      let access_token  = resp ^?! key "access_token" . _String
                          refresh_token = resp ^?! key "refresh_token" . _String
                          expires_in    = fromIntegral (resp ^?! key "expires_in" . _Integral :: Int)
                      return $ session
                        & oas_access_token  .~ access_token
                        & oas_refresh_token .~ refresh_token
                        & oas_expires_in    .~ expires_in
