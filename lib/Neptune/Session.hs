{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TemplateHaskell           #-}
module Neptune.Session where

import           Control.Concurrent        (ThreadId)
import           Control.Concurrent.Event  as E
import           Control.Lens
import           Control.Monad.Except
import qualified Data.Aeson                as Aeson
import           Data.Aeson.Lens
import qualified Data.ByteString.Base64    as Base64
import           Data.Time.Clock           (UTCTime)
import qualified Network.HTTP.Client       as NH
import           RIO                       hiding (Lens', (^.))
import qualified RIO.Text                  as T
import           System.Envy               (FromEnv (..), Parser, env)

import           Neptune.Backend.Core
import           Neptune.Backend.MimeTypes
import           Neptune.Backend.Model     hiding (Experiment, Parameter)
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

data Experiment = Experiment
    { _exp_experiment_id    :: ExperimentId
    , _exp_outbound_q       :: TChan DataPointAny
    , _exp_user_channels    :: ChannelHashMap
    , _exp_stop_flag        :: E.Event
    , _exp_transmitter_flag :: E.Event
    , _exp_transmitter      :: ThreadId
    }

class (Typeable a, Show a) => NeptDataType a where
    neptChannelType :: Proxy a -> ChannelTypeEnum
    toNeptPoint     :: DataPoint a -> Point

newtype DataChannel a = DataChannel Text
    deriving Show

data DataChannelAny = forall a . NeptDataType a => DataChannelAny (DataChannel a)
deriving instance Show DataChannelAny

type ChannelHashMap = TVar (HashMap Text DataChannelAny)

data DataPoint a = DataPoint
    { _dpt_name      :: Text
    , _dpt_timestamp :: UTCTime
    , _dpt_value     :: a
    }
    deriving Show

data DataPointAny = forall a . NeptDataType a => DataPointAny (DataPoint a)
deriving instance Show DataPointAny

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

