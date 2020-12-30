{-|
Module      : Neptune.Client
Description : Neptune Client
Copyright   : (c) Jiasen Wu, 2020
License     : BSD-3-Clause
-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Neptune.Client where

import           Control.Concurrent        (forkIO, killThread)
import           Control.Concurrent.Event  as E (new, set, waitTimeout)
import           Control.Exception         (AsyncException (UserInterrupt),
                                            asyncExceptionFromException, try)
import           Control.Lens              (bimapping, each, filtered, (<&>),
                                            (^.), (^..))
import qualified Data.Text.Lazy            as TL
import qualified Data.Text.Lazy.Encoding   as TL
import           Data.Text.Lens            (packed)
import           Data.Time.Clock           (getCurrentTime)
import qualified Data.UUID                 as UUID (toText)
import           Data.UUID.V4              as UUID (nextRandom)
import qualified Network.HTTP.Client       as NH
import qualified Network.HTTP.Client.TLS   as NH
import           RIO                       hiding (Lens', try, (^.), (^..))
import qualified RIO.HashMap               as M
import qualified RIO.Text                  as T
import           System.Environment        (getArgs, getEnvironment)
import           System.Envy               (decodeEnv)
import           System.Posix.Signals      (Handler (Catch), installHandler,
                                            keyboardSignal)

import           Neptune.AbortHandler      (AbortException (..), abortListener)
import qualified Neptune.Backend.API       as NBAPI
import           Neptune.Backend.Client
import           Neptune.Backend.Core
import           Neptune.Backend.MimeTypes
import           Neptune.Backend.Model     hiding (Experiment, Parameter)
import           Neptune.Backend.ModelLens
import           Neptune.Channel
import           Neptune.OAuth
import           Neptune.Session
import           Neptune.Utils


-- | Experiment's hyper-parameter. When creating an experiment, you could
-- specify parameters to present in the web console.
data Parameter = ExperimentParamS Text Text
    | ExperimentParamD Text Double

-- | Create an experiment
createExperiment :: HasCallStack
                 => NeptuneSession -- ^ Session
                 -> Maybe Text -- ^ Optional name (automatically assigned if Nothing)
                 -> Maybe Text -- ^ Optional description
                 -> [Parameter] -- ^ hyper-parameters
                 -> [(Text, Text)] -- ^ properties
                 -> [Text] -- ^ tags
                 -> IO Experiment
createExperiment session@NeptuneSession{..} name description params props tags = do
    -- TODO support git_info
    -- TODO support uploading source code
    -- TODO support abort callback. W/o a callback, the app will
    --      continue running when you click abort in the web console
    params <- mapM _mkParameter params
    exp    <- _neptune_dispatch $ NBAPI.createExperiment
                (ContentType MimeJSON)
                (Accept MimeJSON)
                (mkExperimentCreationParams
                    (_neptune_project ^. projectWithRoleDTOIdL)
                    (map (uncurry KeyValueProperty) props)
                    "" -- legacy
                    params
                    "command" -- legacy
                    (fromMaybe "Untitled" name)
                    tags){ experimentCreationParamsDescription = description
                         , experimentCreationParamsAbortable = Just True }

    let exp_id = ExperimentId (exp ^. experimentIdL)
    chan <- newTChanIO
    user_channels <- newTVarIO M.empty
    stop_flag <- E.new
    transmitter_flag <- E.new
    let exp = Experiment exp_id chan user_channels stop_flag transmitter_flag undefined undefined
    transmitter_thread <- forkIO $ transmitter session exp

    parent_thread <- myThreadId
    abort_handler <- forkIO $ abortListener session exp parent_thread

    return exp {_exp_transmitter = transmitter_thread, _exp_abort_handler = abort_handler}

    where
        _mkParameter (ExperimentParamS name value) = do
            _id <- UUID.toText <$> UUID.nextRandom
            return $ mkParameter name ParameterTypeEnum'String _id value
        _mkParameter (ExperimentParamD name value) = do
            _id <- UUID.toText <$> UUID.nextRandom
            return $ mkParameter name ParameterTypeEnum'Double _id (tshow value)

-- | Log a key-value pair
nlog :: (HasCallStack, NeptDataType a)
     => Experiment -- ^ experiment
     -> Text -- ^ key
     -> a -- ^ value
     -> IO ()
nlog exp name value = do
    now <- getCurrentTime
    let chan = exp ^. exp_outbound_q
        dat  = DataPointAny $ DataPoint name now value
    atomically $ writeTChan chan dat

-- | Run an action within a neptune session and a new experiment
withNept :: Text -- ^ \<namespace\>\/\<project_name\>
         -> (NeptuneSession -> Experiment -> IO a) -- ^ action
         -> IO a
withNept project_qualified_name act = do
    args <- T.unwords . map T.pack <$> getArgs
    envs <- getEnvironment
    let valid_pat name = T.isPrefixOf "MXNET_"  name ||
                         T.isPrefixOf "NVIDIA_" name ||
                         T.isPrefixOf "CUDA_"   name
    envs <- pure $ envs ^.. each . bimapping packed packed . filtered (valid_pat . fst)
    withNept' project_qualified_name Nothing Nothing [] (("args", args) : envs) [] act

-- | Run an action within a neptune session and a new experiment
withNept' :: Text -- ^ \<namespace\>\/\<project_name\>
          -> Maybe Text -- ^ Optional name of the experiment (automatically assigned if Nothing)
          -> Maybe Text -- ^ Optional description of the experiment
          -> [Parameter] -- ^ experiment hyper-parameters
          -> [(Text, Text)] -- ^ experiment properties
          -> [Text] -- ^ experiment tags
          -> (NeptuneSession -> Experiment -> IO a) -- ^ action
          -> IO a
withNept' project_qualified_name name description params props tags act = do
    ses <- initNept project_qualified_name
    exp <- createExperiment ses name description params props tags

    -- install an signal handler for CTRL-C, ensuring that an async-
    -- exception UserInterrupt is sent to the main thread
    main_thread <- myThreadId
    let interrupted = throwTo main_thread UserInterrupt

    old_handler <- installHandler keyboardSignal (Catch interrupted) Nothing
    result <- try (act ses exp)
    _ <- installHandler keyboardSignal old_handler Nothing

    case result of
      Left (e :: SomeException) -> do
          let end_state = case fromException e of
                            Just AbortException -> Nothing
                            _ -> Just $
                                case asyncExceptionFromException e of
                                  Just UserInterrupt -> (ExperimentState'Failed, "User interrupted.")
                                  _ -> (ExperimentState'Failed, T.pack $ displayException e)
          teardownNept ses exp end_state
          throwM e
      Right a -> do
          teardownNept ses exp (Just (ExperimentState'Succeeded, ""))
          return a

-- | Initialize a neptune session
initNept :: HasCallStack
         => Text -- ^ \<namespace\>\/\<project_name\>
         -> IO NeptuneSession
initNept project_qualified_name = do
    ct@ClientToken{..} <- decodeEnv >>= either throwString return

    mgr <- NH.newManager NH.tlsManagerSettings
    config0 <- pure . withNoLogging =<< newConfig

    let api_endpoint = TL.encodeUtf8 (TL.fromStrict _ct_api_url)
        config = config0 { configHost = api_endpoint }

    let dispatch = dispatchMime mgr config{configValidateAuthMethods = False}
                    >=> handleMimeError

    oauth_token <- dispatch  $ NBAPI.exchangeApiToken (Accept MimeJSON) (XNeptuneApiToken _ct_token)
    (refresh_thread, oauth_session) <- oauth2Setup (oauth_token ^. neptuneOauthTokenAccessTokenL)
                                                   (oauth_token ^. neptuneOauthTokenRefreshTokenL)

    -- TODO there is a chance that the access token gets invalid right after readMVar
    let dispatch :: (HasCallStack, Produces req accept, MimeUnrender accept res, MimeType contentType)
                 => NeptuneBackendRequest req contentType res accept -> IO res
        dispatch req = do
            access_token <- readMVar oauth_session <&> _oas_access_token
            resp <- dispatchMime mgr (config `addAuthMethod` AuthOAuthOauth2 access_token) req
            handleMimeError resp

    proj <- dispatch $ NBAPI.getProject (Accept MimeJSON) (ProjectIdentifier project_qualified_name)
    return $ NeptuneSession
        { _neptune_http_manager = mgr
        , _neptune_client_token = ct
        , _neptune_config = config
        , _neptune_oauth2 = oauth_session
        , _neptune_oauth2_refresh = refresh_thread
        , _neptune_project = proj
        , _neptune_dispatch = dispatch
        }

-- | Teardown a neptune session
teardownNept :: NeptuneSession -- ^ session
             -> Experiment -- ^ experiment
             -> Maybe (ExperimentState, Text) -- ^ completion state & message
             -> IO ()
teardownNept NeptuneSession{..} experiment state_msg = do
    E.set (experiment ^. exp_stop_flag)
    -- wait at most 5 seconds
    done <- E.waitTimeout (experiment ^. exp_transmitter_flag) 5000000
    -- kill if timeout
    unless done $
        killThread $ experiment ^. exp_transmitter
    killThread $ _neptune_oauth2_refresh

    case state_msg of
      Just (state, msg) ->
          void (_neptune_dispatch $ NBAPI.markExperimentCompleted
                    (ContentType MimeJSON)
                    (Accept MimeNoContent)
                    (mkCompletedExperimentParams state msg)
                    (experiment ^. exp_experiment_id) :: IO NoContent)
      Nothing -> return ()
