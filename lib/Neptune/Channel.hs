{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}
module Neptune.Channel where

import           Control.Concurrent.Event  as E
import           Control.Lens
import           Data.Time.Clock           (UTCTime)
import           Data.Time.Clock.POSIX     (utcTimeToPOSIXSeconds)
import           Data.Typeable
import           RIO                       hiding (Lens', (^.))
import qualified RIO.HashMap               as M

import qualified Neptune.Backend.API       as NBAPI
import           Neptune.Backend.MimeTypes
import           Neptune.Backend.Model     hiding (Experiment)
import           Neptune.Backend.ModelLens
import           Neptune.Session

data DataChannelWithData = forall a. NeptDataType a => DataChannelWithData (DataChannel a, [DataPoint a])

transmitter :: HasCallStack
            => NeptuneSession
            -> Experiment
            -> IO ()
transmitter session@NeptuneSession{..} Experiment{..} = go
    where
        go = do
            stop_flag <- E.isSet _exp_stop_flag
            dat <- atomically $
                    if stop_flag
                      then readTChanFull _exp_outbound_q
                      else readTChanAtMost 100 _exp_outbound_q
            send dat
            if not stop_flag
               then go
               else E.set _exp_transmitter_flag

        send dat = do
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
                                _exp_user_channels
                                chn_name
                                (createChannel session _exp_experiment_id _exp_user_channels chn_name)

                case chn of
                  DataChannelAny chn -> do
                      let (errs, grouped) = gatherDataPoints (proxy chn) dat
                      -- TODO log properly
                      mapM_ print errs
                      return $ DataChannelWithData (chn, grouped)

            sendChannel session _exp_experiment_id chn_with_dat

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

readTChanFull :: TChan a -> STM [a]
readTChanFull chan = do
    vs <- tryReadTChan chan
    case vs of
      Nothing -> return []
      Just v -> do
          vs <- readTChanFull chan
          return $ v : vs

gatherDataPoints :: forall a. NeptDataType a
                 => Proxy a
                 -> [DataPointAny]
                 -> ([Text], [DataPoint a])
gatherDataPoints _ dpa = partitionEithers $ map castData dpa
    where
        castData (DataPointAny d) = case cast d of
                                      Nothing -> Left "?? is not compatible for the channel"
                                      Just o -> Right o
