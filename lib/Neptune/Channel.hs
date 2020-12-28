{-|
Module      : Neptune.Channel
Description : Neptune Client
Copyright   : (c) Jiasen Wu, 2020
License     : BSD-3-Clause
-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
module Neptune.Channel where

import           Control.Concurrent.Event  as E (isSet, set)
import           Control.Lens
import           Control.Retry             (recoverAll, retryPolicyDefault)
import           Data.Typeable             (Proxy (..), cast)
import           RIO                       hiding (Lens', (.~), (^.), (^..),
                                            (^?))
import qualified RIO.HashMap               as M

import qualified Neptune.Backend.API       as NBAPI
import           Neptune.Backend.Core      (configLogContext,
                                            configLogExecWithContext)
import           Neptune.Backend.Logging   (_log, levelError)
import           Neptune.Backend.MimeTypes
import           Neptune.Backend.Model     hiding (Experiment)
import           Neptune.Backend.ModelLens
import           Neptune.Session

data DataChannelWithData = forall a. NeptDataType a => DataChannelWithData (DataChannel a, [DataPoint a])

-- | Background thread for transmission
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
            -- Retry sending the whole batch upto 6 times.
            -- Discard and log if fails continuously
            handle onErr $
                recoverAll retryPolicyDefault $ \_ ->
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
                                (createChannel session _exp_experiment_id chn_name)

                case chn of
                  DataChannelAny chn -> do
                      let (errs, grouped) = gatherDataPoints (proxy chn) dat
                      mapM_ (logE _neptune_config) errs
                      return $ DataChannelWithData (chn, grouped)

            sendChannel session _exp_experiment_id chn_with_dat

        onErr exc = logE _neptune_config $ "Failed to send data points.\n" <> tshow (exc :: SomeException)

        proxy :: f a -> Proxy a
        proxy _ = Proxy

-- | Create a neptune data channel.
createChannel :: forall t. (NeptDataType t, HasCallStack)
              => NeptuneSession -> ExperimentId -> Text -> IO (DataChannel t)
createChannel NeptuneSession{..} exp_id chn_name = do
    -- call create channel api
    let chn_type = neptChannelType  (Proxy :: Proxy t)
    chn <- _neptune_dispatch $ NBAPI.createChannel
        (ContentType MimeJSON)
        (Accept MimeJSON)
        (mkChannelParams chn_name chn_type)
        exp_id
    return $ DataChannel (chn ^. channelDTOIdL) :: IO (DataChannel t)

-- | Get a neptune data channel. If the data channel doesn't exist yet,
-- a data channel will be created and added to the hashmap of current
-- user channels.
getOrCreateChannel :: forall t. NeptDataType t
                   => Proxy t -- ^ dummy data type
                   -> ChannelHashMap -- ^ current user channels
                   -> Text -- ^ channel name
                   -> IO (DataChannel t) -- ^ creator
                   -> IO DataChannelAny
getOrCreateChannel _ user_channels chn_name creator = do
    uc  <- readTVarIO user_channels
    case uc ^? ix chn_name of
      Nothing -> do
        -- add to `user_channels`
        chn_new <- DataChannelAny <$> creator
        atomically $ do
            mapping <- readTVar user_channels
            writeTVar user_channels (mapping & ix chn_name .~ chn_new)
            return chn_new
      Just x  -> return x

-- | Send a batch of data in their respective channel.
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
        logE _neptune_config $ tshow (chn, xs, ecode, emsg)

    where
        toChannelsValues :: DataChannelWithData -> InputChannelValues
        toChannelsValues (DataChannelWithData (DataChannel chn, dat)) =
            mkInputChannelValues chn (map toNeptPoint dat)

-- | Read at most 'n' items from the queue, with blocking read at the
-- first item.
readTChanAtMost :: Int -> TChan a -> STM [a]
readTChanAtMost n chan = do
    -- blocking-read for the 1st elem
    -- i.e. wait until there is sth
    v0 <- readTChan chan
    vs <- sequence $ replicate (n - 1) (tryReadTChan chan)
    return $ v0 : catMaybes vs

-- | Read all items from the queue
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
                                      Nothing -> Left $ _dpt_name d <> " is not compatible for the channel"
                                      Just o -> Right o

logE config msg = configLogExecWithContext config (configLogContext config) $
    _log "Neptune.Channel" levelError msg
