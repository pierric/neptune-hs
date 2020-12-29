{-|
Module      : Neptune.Session
Description : Neptune Client
Copyright   : (c) Jiasen Wu, 2020
License     : BSD-3-Clause
-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TemplateHaskell           #-}
module Neptune.Session where

import           Control.Concurrent        (ThreadId)
import           Control.Concurrent.Event  as E (Event)
import           Control.Lens
import           Control.Monad.Except      (MonadError (throwError))
import qualified Data.Aeson                as Aeson
import           Data.Aeson.Lens
import qualified Data.ByteString.Base64    as Base64
import           Data.Time.Clock           (UTCTime)
import           Data.Time.Clock.POSIX     (utcTimeToPOSIXSeconds)
import qualified Network.HTTP.Client       as NH
import           RIO                       hiding (Lens', (^.))
import qualified RIO.Text                  as T
import           System.Envy               (FromEnv (..), Parser, env)

import           Neptune.Backend.Core
import           Neptune.Backend.MimeTypes
import           Neptune.Backend.Model     hiding (Experiment, Parameter)
import           Neptune.OAuth


-- | Decoded client token
data ClientToken = ClientToken
    { _ct_token       :: Text -- ^ user secret
    , _ct_api_address :: Text -- ^ neptune api address
    , _ct_api_url     :: Text -- ^ neptune api address
    , _ct_api_key     :: Text -- ^ neptune api key (not used)
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

-- | Neptune session. It contains all necessary information to communicate the
-- server.
data NeptuneSession = NeptuneSession
    { _neptune_http_manager   :: NH.Manager
    , _neptune_client_token   :: ClientToken
    , _neptune_config         :: NeptuneBackendConfig
    , _neptune_oauth2         :: MVar OAuth2Session
    , _neptune_oauth2_refresh :: ThreadId -- ^ Background thread for updating OAuth2 token
    , _neptune_project        :: ProjectWithRoleDTO -- ^ Active project
    , _neptune_dispatch       :: Dispatcher -- ^ Dispatching function for http requests
    }

data Experiment = Experiment
    { _exp_experiment_id    :: ExperimentId -- ^ Experiment Id
    , _exp_outbound_q       :: TChan DataPointAny -- ^ Output queue
    , _exp_user_channels    :: ChannelHashMap -- ^ Active output channels
    , _exp_stop_flag        :: E.Event -- ^ Event flag to indicate the end of the session
    , _exp_transmitter_flag :: E.Event -- ^ Event flag to indicate completion of transmission
    , _exp_transmitter      :: ThreadId -- ^ Background thread for transmission
    , _exp_abort_handler    :: ThreadId
    }

class (Typeable a, Show a) => NeptDataType a where
    neptChannelType :: Proxy a -> ChannelTypeEnum
    toNeptPoint     :: DataPoint a -> Point

-- | Type-safe data channel
newtype DataChannel a = DataChannel Text {- ^ Channel Id -}
    deriving Show

-- | Data channel of any type
data DataChannelAny = forall a . NeptDataType a => DataChannelAny (DataChannel a)
deriving instance Show DataChannelAny

-- | Hashmap of all channels in use
type ChannelHashMap = TVar (HashMap Text DataChannelAny)

-- | Type-safe data point
data DataPoint a = DataPoint
    { _dpt_name      :: Text
    , _dpt_timestamp :: UTCTime
    , _dpt_value     :: a
    }
    deriving Show

-- | Data point of any type
data DataPointAny = forall a . NeptDataType a => DataPointAny (DataPoint a)
deriving instance Show DataPointAny

makeLenses ''ClientToken
makeLensesFor [("_neptune_http_manager", "neptune_http_manager")
              ,("_neptune_client_token", "neptune_client_token")
              ,("_neptune_config", "neptune_config")
              ,("_neptune_oauth2", "neptune_oauth2")
              ,("_neptune_oauth2_refresh", "neptune_oauth2_refresh")
              ,("_neptune_project", "neptune_project")]
              ''NeptuneSession
makeLenses ''Experiment
makeLenses ''DataPoint

-- | Lens to get/set '_dpt_name' for 'DataPointAny'
dpt_name_A :: Lens' DataPointAny Text
dpt_name_A f (DataPointAny (DataPoint n t v)) =
    let set = (\n -> DataPointAny (DataPoint n t v))
     in set <$> f n

-- | Lens to get/set '_dpt_timestamp' for 'DataPointAny'
dpt_timestamp_A :: Lens' DataPointAny UTCTime
dpt_timestamp_A f (DataPointAny (DataPoint n t v)) =
    let set = (\t -> DataPointAny (DataPoint n t v))
     in set <$> f t

instance NeptDataType Double where
    neptChannelType  _ = ChannelTypeEnum'Numeric
    toNeptPoint dat    = let t = floor $ utcTimeToPOSIXSeconds (dat ^. dpt_timestamp) * 1000
                             y = mkY{ yNumericValue = dat ^. dpt_value . re _Just }
                          in mkPoint t y


