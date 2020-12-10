{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TemplateHaskell           #-}
module Neptune.Channel where

import           Control.Lens
import           Data.Time.Clock           (UTCTime)
import           Data.Time.Clock.POSIX     (utcTimeToPOSIXSeconds)
import           Data.Typeable
import           RIO                       hiding (Lens', (^.))
import qualified RIO.HashMap               as M

import qualified Neptune.Backend.API       as NBAPI
import           Neptune.Backend.MimeTypes
import           Neptune.Backend.Model
import           Neptune.Backend.ModelLens
import           Neptune.Session

class (Typeable a, Show a) => NeptDataType a where
    neptChannelType :: Proxy a -> ChannelTypeEnum
    toNeptPoint     :: DataPoint a -> Point

data DataPoint a = DataPoint
    { _dpt_name      :: Text
    , _dpt_timestamp :: UTCTime
    , _dpt_value     :: a
    }
    deriving Show

data DataPointAny = forall a . NeptDataType a => DataPointAny (DataPoint a)
deriving instance Show DataPointAny

newtype DataChannel a = DataChannel Text
    deriving Show

data DataChannelAny = forall a . NeptDataType a => DataChannelAny (DataChannel a)
deriving instance Show DataChannelAny

data DataChannelWithData = forall a. NeptDataType a => DataChannelWithData (DataChannel a, [DataPoint a])

type ChannelHashMap = TVar (HashMap Text DataChannelAny)

makeLenses ''DataPoint

dpt_name_A :: Lens' DataPointAny Text
dpt_name_A f (DataPointAny (DataPoint n t v)) =
    let set = (\n -> DataPointAny (DataPoint n t v))
     in set <$> f n

dpt_timestamp_A :: Lens' DataPointAny UTCTime
dpt_timestamp_A f (DataPointAny (DataPoint n t v)) =
    let set = (\t -> DataPointAny (DataPoint n t v))
     in set <$> f t

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
    chn <- _neptune_dispatch $ NBAPI.createChannel
        (ContentType MimeJSON)
        (Accept MimeJSON)
        (mkChannelParams chn_name chn_type)
        exp_id
    let chn_new = DataChannel (chn ^. channelDTOIdL) :: DataChannel t
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
    errors <- _neptune_dispatch $ NBAPI.postChannelValues
                (ContentType MimeJSON)
                (Accept MimeJSON)
                (ChannelsValues $ map toChannelsValues chn'value)
                exp_id
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
