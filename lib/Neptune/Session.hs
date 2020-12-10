{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE Rank2Types      #-}
{-# LANGUAGE TemplateHaskell #-}
module Neptune.Session where

import           Control.Concurrent        (ThreadId)
import           Control.Lens
import           Control.Monad.Except
import qualified Data.Aeson                as Aeson
import           Data.Aeson.Lens
import qualified Data.ByteString.Base64    as Base64
import qualified Network.HTTP.Client       as NH
import           RIO                       hiding (Lens', (^.))
import qualified RIO.Text                  as T
import           System.Envy               (FromEnv (..), Parser, env)

import           Neptune.Backend.Core
import           Neptune.Backend.MimeTypes
import           Neptune.Backend.Model
import           Neptune.OAuth

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


type Dispatcher = forall req contentType res accept .
                  (Produces req accept, MimeUnrender accept res, MimeType contentType, HasCallStack)
                  => NeptuneBackendRequest req contentType res accept
                  -> IO res

data NeptuneSession = NeptuneSession
    { _neptune_http_manager   :: NH.Manager
    , _neptune_client_token   :: ClientToken
    , _neptune_config         :: NeptuneBackendConfig
    , _neptune_oauth2         :: MVar OAuth2Session
    , _neptune_oauth2_refresh :: ThreadId
    , _neptune_project        :: ProjectWithRoleDTO
    , _neptune_dispatch       :: Dispatcher
    }

