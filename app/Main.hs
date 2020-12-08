{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE ViewPatterns              #-}
module Main where

import           Control.Concurrent         (forkIO)
import           Control.Lens
import           Control.Monad.Except
import qualified Data.Aeson                 as Aeson
import           Data.Aeson.Lens
import qualified Data.ByteString.Base64     as Base64
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.Text.Lazy             as TL
import qualified Data.Text.Lazy.Encoding    as TL
import           Data.Time.Clock            (NominalDiffTime, UTCTime,
                                             getCurrentTime)
import           Data.Time.Clock.POSIX      (getPOSIXTime,
                                             utcTimeToPOSIXSeconds)
import           Data.Typeable
import qualified Data.UUID                  as UUID (toText)
import           Data.UUID.V4               as UUID
import           GHC.Generics
import qualified Network.HTTP.Client        as NH
import qualified Network.HTTP.Client.TLS    as NH
import           Network.HTTP.Req
import           RIO                        hiding (Lens', (^.))
import qualified RIO.HashMap                as M
import qualified RIO.Text                   as T
import           System.Envy                (FromEnv (..), Parser, decodeEnv,
                                             env)
import           Text.URI                   (mkURI)
import qualified Web.JWT                    as JWT

import qualified NeptuneBackend             as NB
import           NeptuneBackend.Client
import           NeptuneBackend.Core
import           NeptuneBackend.MimeTypes
import           NeptuneBackend.Model       hiding (Experiment, Parameter)
import           NeptuneBackend.ModelLens

data ClientToken = ClientToken
    { _ct_token       :: Text
    , _ct_api_address :: Text
    , _ct_api_url     :: Text
    , _ct_api_key     :: Text
    }
    deriving (Generic, Show)

instance FromEnv ClientToken where
    fromEnv _ = do
        token <- env "NEPTUNE_API_TOKEN"
        jsraw <- either throwError return $ Base64.decode $ T.encodeUtf8 token
        js    <- either throwError return $
                    Aeson.eitherDecodeStrict' jsraw :: Parser Aeson.Value
        return $ ClientToken
            token
            (js ^?! key "api_address" . _String)
            (js ^?! key "api_url" . _String)
            (js ^?! key "api_key" . _String)


data OAuth2Session = OAuth2Session
    { _oas_client_id     :: Text
    , _oas_access_token  :: Text
    , _oas_refresh_token :: Text
    , _oas_expires_in    :: NominalDiffTime
    , _oas_refresh_url   :: Text
    }

makeLenses ''OAuth2Session

handleMimeError :: (Monad m, HasCallStack) => MimeResult a -> m a
handleMimeError (mimeResult -> Left e)  = let err_msg = mimeError e
                                              response = NH.responseBody $ mimeErrorResponse e
                                           in error (mimeError e ++ " " ++ BSL.unpack response)
handleMimeError (mimeResult -> Right r) = return r

oauth2_setup :: Text -> Text -> IO (MVar OAuth2Session)
oauth2_setup access_token refresh_token = do
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
    _ <- forkIO (oauth_refresher oauth_session_var)
    return oauth_session_var

oauth_refresher :: MVar OAuth2Session -> IO ()
oauth_refresher session_var = update >> oauth_refresher session_var
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
                          expires_in    = fromIntegral (resp ^?! key "expires_in" . _Integral)
                      return $ session
                        & oas_access_token  .~ access_token
                        & oas_refresh_token .~ refresh_token
                        & oas_expires_in    .~ expires_in

type Dispatcher = forall req contentType res accept .
                  (Produces req accept, MimeUnrender accept res, MimeType contentType)
                  => NeptuneBackendRequest req contentType res accept
                  -> IO res

data NeptuneSession = NeptuneSession
    { _neptune_http_manager :: NH.Manager
    , _neptune_client_token :: ClientToken
    , _neptune_config       :: NeptuneBackendConfig
    , _neptune_oauth2       :: MVar OAuth2Session
    , _neptune_project      :: ProjectWithRoleDTO
    , _neptune_dispatch     :: Dispatcher
    }

initNept :: HasCallStack => Text -> IO NeptuneSession
initNept project_qualified_name = do
    ct@ClientToken{..} <- decodeEnv >>= either throwString return

    mgr <- NH.newManager NH.tlsManagerSettings
    config0 <- withStderrLogging =<< newConfig

    let api_endpoint = TL.encodeUtf8 (TL.fromStrict _ct_api_url)
        config = config0 { NB.configHost = api_endpoint }

    let dispatch = dispatchMime mgr config{NB.configValidateAuthMethods = False}
                    >=> handleMimeError

    oauth_token   <- dispatch  $ NB.exchangeApiToken (Accept MimeJSON) (XNeptuneApiToken _ct_token)
    oauth_session <- oauth2_setup
                        (oauth_token ^. neptuneOauthTokenAccessTokenL)
                        (oauth_token ^. neptuneOauthTokenRefreshTokenL)

    -- TODO there is a chance that the access token gets invalid right after readMVar
    let dispatch :: (HasCallStack, Produces req accept, MimeUnrender accept res, MimeType contentType)
                 => NeptuneBackendRequest req contentType res accept -> IO res
        dispatch req = do
            access_token <- readMVar oauth_session <&> _oas_access_token
            resp <- dispatchMime mgr (config `addAuthMethod` AuthOAuthOauth2 access_token) req
            handleMimeError resp

    proj <- dispatch $ NB.getProject (Accept MimeJSON) (ProjectIdentifier project_qualified_name)
    return $ NeptuneSession
        { _neptune_http_manager = mgr
        , _neptune_client_token = ct
        , _neptune_config = config
        , _neptune_oauth2 = oauth_session
        , _neptune_project = proj
        , _neptune_dispatch = dispatch
        }

data Parameter = ExperimentParamS Text Text
    | ExperimentParamD Text Double

data Experiment = Experiment
    { _exp_experiment_id :: ExperimentId
    , _exp_outbound_q    :: TChan DataPointAny
    , _exp_user_channels :: ChannelHashMap
    }


class (Typeable a, Show a) => NeptDataType a where
    neptChannelType :: Proxy a -> ChannelTypeEnum
    toNeptPoint     :: DataPoint a -> Point

data DataPointAny = forall a . NeptDataType a => DataPointAny (DataPoint a)
deriving instance Show DataPointAny

data DataChannelAny = forall a . NeptDataType a => DataChannelAny (DataChannel a)
deriving instance Show DataChannelAny

data DataChannelWithData = forall a. NeptDataType a => DataChannelWithData (DataChannel a, [DataPoint a])

newtype DataChannel a = DataChannel Text
    deriving Show

type ChannelHashMap = TVar (HashMap Text DataChannelAny)

data DataPoint a = DataPoint
    { _dpt_name      :: Text
    , _dpt_timestamp :: UTCTime
    , _dpt_value     :: a
    }
    deriving Show

makeLenses ''Experiment
makeLenses ''DataPoint

dpt_name_A :: Lens' DataPointAny Text
dpt_name_A f (DataPointAny (DataPoint n t v)) =
    let set = (\n -> DataPointAny (DataPoint n t v))
     in set <$> f n

dpt_timestamp_A :: Lens' DataPointAny UTCTime
dpt_timestamp_A f (DataPointAny (DataPoint n t v)) =
    let set = (\t -> DataPointAny (DataPoint n t v))
     in set <$> f t

createExperiment :: HasCallStack
                 => NeptuneSession
                 -> Maybe Text
                 -> Maybe Text
                 -> [Parameter]
                 -> [(Text, Text)]
                 -> [Text]
                 -> IO Experiment
createExperiment session@NeptuneSession{..} name description params props tags = do
    -- TODO support git_info
    -- TODO support uploading source code
    -- TODO support abort callback. W/o a callback, the app will
    --      continue running when you click abort in the web console
    params <- mapM _mkParameter params
    print ("@@@@@@@@@", name)
    exp    <- _neptune_dispatch $ NB.createExperiment
                (ContentType MimeJSON)
                (Accept MimeJSON)
                (mkExperimentCreationParams
                    (_neptune_project ^. projectWithRoleDTOIdL)
                    (map (uncurry KeyValueProperty) props)
                    "" -- legacy
                    params
                    "command" -- legacy
                    (fromMaybe "Untitled" name)
                    tags){ experimentCreationParamsDescription = description }
    print ("@######")
    let exp_id = ExperimentId (exp ^. experimentIdL)
    chan <- newTChanIO
    user_channels <- newTVarIO M.empty
    _ <- forkIO $ transmitter session exp_id chan user_channels
    return $ Experiment exp_id chan user_channels

    where
        _mkParameter (ExperimentParamS name value) = do
            _id <- UUID.toText <$> UUID.nextRandom
            return $ mkParameter name ParameterTypeEnum'String _id value
        _mkParameter (ExperimentParamD name value) = do
            _id <- UUID.toText <$> UUID.nextRandom
            return $ mkParameter name ParameterTypeEnum'Double _id (tshow value)

nlog :: (HasCallStack, NeptDataType a) => Experiment -> Text -> a -> IO ()
nlog exp name value = do
    now <- getCurrentTime
    let chan = exp ^. exp_outbound_q
        dat  = DataPointAny $ DataPoint name now value
    atomically $ writeTChan chan dat

transmitter :: HasCallStack
            => NeptuneSession
            -> ExperimentId
            -> TChan DataPointAny
            -> ChannelHashMap
            -> IO ()
transmitter session@NeptuneSession{..} exp_id chan user_channels = sequence_ (repeat go)
    where
        go = do dat <- atomically $ readTChanAtMost 10 chan

                let dup           = Control.Lens.to (\a -> (a, a))
                    singleton     = Control.Lens.to (\a -> [a])
                    merge         = M.toList . foldl' (M.unionWith (++)) M.empty . map (uncurry M.singleton)
                    dat_with_name :: [(Text, [DataPointAny])]
                    dat_with_name = merge $ dat ^.. traverse . dup . alongside dpt_name_A singleton

                print dat_with_name

                chn_with_dat <- forM dat_with_name $ \(chn_name, dat) -> do
                    -- If channel doesn't exist, then we create one with
                    -- channel type from the first element.
                    chn <- case head dat of
                             DataPointAny d0 ->
                                 getOrCreateChannel
                                    (proxy d0)
                                    user_channels
                                    chn_name
                                    (createChannel session exp_id user_channels chn_name)

                    case chn of
                      DataChannelAny chn -> do
                          let (errs, grouped) = gatherDataPoints (proxy chn) dat
                          -- TODO log properly
                          mapM_ print errs
                          return $ DataChannelWithData (chn, grouped)

                sendChannel session exp_id chn_with_dat

        proxy :: f a -> Proxy a
        proxy _ = Proxy


instance NeptDataType Double where
    neptChannelType  _ = ChannelTypeEnum'Numeric
    toNeptPoint dat    = let t = floor $ utcTimeToPOSIXSeconds (dat ^. dpt_timestamp) * 1000
                             y = mkY{ yNumericValue = dat ^. dpt_value . re _Just }
                          in mkPoint t y

createChannel :: forall t. (NeptDataType t, HasCallStack)
              => NeptuneSession -> ExperimentId -> ChannelHashMap -> Text -> IO (DataChannel t)
createChannel NeptuneSession{..} exp_id user_channels chn_name = do
    -- call create channel api
    let chn_type = neptChannelType  (Proxy :: Proxy t)
    print ("@@@@", exp_id, chn_name)
    chn <- _neptune_dispatch $ NB.createChannel
        (ContentType MimeJSON)
        (Accept MimeJSON)
        (mkChannelParams chn_name chn_type)
        exp_id
    print "##@@@"
    let chn_new = DataChannel (chn ^. channelDTOIdL)
    -- add to `user_channels`
    atomically $ do
        mapping <- readTVar user_channels
        writeTVar user_channels (mapping & ix chn_name .~ (DataChannelAny chn_new))
    return chn_new

getOrCreateChannel :: forall t. NeptDataType t
                   => Proxy t
                   -> ChannelHashMap
                   -> Text
                   -> IO (DataChannel t)
                   -> IO DataChannelAny
getOrCreateChannel _ user_channels chn_name creator = do
    uc  <- readTVarIO user_channels
    case uc ^? ix chn_name of
      Nothing -> DataChannelAny <$> creator
      Just x  -> return x

sendChannel :: HasCallStack => NeptuneSession -> ExperimentId -> [DataChannelWithData] -> IO ()
sendChannel NeptuneSession{..} exp_id chn'value = do
    print ("@@@@@", exp_id, length chn'value)
    errors <- _neptune_dispatch $ NB.postChannelValues
                (ContentType MimeJSON)
                (Accept MimeJSON)
                (ChannelsValues $ map toChannelsValues chn'value)
                exp_id
    print "####@"
    -- TODO proper logging
    forM_ errors $ \err -> do
        let chn   = err ^. batchChannelValueErrorDTOChannelIdL
            xs    = err ^. batchChannelValueErrorDTOXL
            ecode = err ^. batchChannelValueErrorDTOErrorL . errorCodeL
            emsg  = err ^. batchChannelValueErrorDTOErrorL . errorMessageL
        print (chn, xs, ecode, emsg)

    where
        toChannelsValues :: DataChannelWithData -> InputChannelValues
        toChannelsValues (DataChannelWithData (DataChannel chn, dat)) =
            mkInputChannelValues chn (map toNeptPoint dat)


readTChanAtMost :: Int -> TChan a -> STM [a]
readTChanAtMost n chan = do
    -- blocking-read for the 1st elem
    -- i.e. wait until there is sth
    v0 <- readTChan chan
    vs <- sequence $ replicate (n - 1) (tryReadTChan chan)
    return $ v0 : catMaybes vs

gatherDataPoints :: forall a. NeptDataType a
                 => Proxy a
                 -> [DataPointAny]
                 -> ([Text], [DataPoint a])
gatherDataPoints _ dpa = partitionEithers $ map castData dpa
    where
        castData (DataPointAny d) = case cast d of
                                      Nothing -> Left "?? is not compatible for the channel"
                                      Just o -> Right o

main = do
    session@NeptuneSession{..} <- initNept "jiasen/sandbox"
    exp <- createExperiment session Nothing Nothing [] [] []
    forM_ [1..10::Int] $ \i -> do
        nlog exp "counter" (fromIntegral (i * i) :: Double)
        threadDelay 1000

    -- resp <- _neptune_dispatch $
    --             NB.listProjects (Accept MimeJSON)
    --             NB.-&- OrganizationIdentifier  "jiasen"
    -- print resp
