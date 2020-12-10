{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Neptune.Client where

import           Control.Concurrent        (ThreadId, forkIO, killThread)
import           Control.Lens
import qualified Data.Text.Lazy            as TL
import qualified Data.Text.Lazy.Encoding   as TL
import           Data.Time.Clock           (getCurrentTime)
import qualified Data.UUID                 as UUID (toText)
import           Data.UUID.V4              as UUID
import qualified Network.HTTP.Client       as NH
import qualified Network.HTTP.Client.TLS   as NH
import           RIO                       hiding (Lens', (^.))
import qualified RIO.HashMap               as M
import qualified RIO.Text                  as T
import           System.Envy               (decodeEnv)

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


data Parameter = ExperimentParamS Text Text
    | ExperimentParamD Text Double

data Experiment = Experiment
    { _exp_experiment_id :: ExperimentId
    , _exp_outbound_q    :: TChan DataPointAny
    , _exp_user_channels :: ChannelHashMap
    , _exp_transmitter   :: ThreadId
    }

makeLenses ''Experiment


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
                    tags){ experimentCreationParamsDescription = description }
    let exp_id = ExperimentId (exp ^. experimentIdL)
    chan <- newTChanIO
    user_channels <- newTVarIO M.empty
    transmitter_thread <- forkIO $ transmitter session exp_id chan user_channels
    return $ Experiment exp_id chan user_channels transmitter_thread

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

withNept :: Text -> (NeptuneSession -> Experiment -> IO a) -> IO a
withNept project_qualified_name act = do
    ses <- initNept project_qualified_name
    exp <- createExperiment ses Nothing Nothing [] [] []

    result <- try (act ses exp)
    case result of
      Left (e :: SomeException) -> do
          teardownNept ses exp ExperimentState'Failed (T.pack $ displayException e)
          throwM e
      Right a -> do
          teardownNept ses exp ExperimentState'Succeeded ""
          return a

initNept :: HasCallStack => Text -> IO NeptuneSession
initNept project_qualified_name = do
    ct@ClientToken{..} <- decodeEnv >>= either throwString return

    mgr <- NH.newManager NH.tlsManagerSettings
    config0 <- withStderrLogging =<< newConfig

    let api_endpoint = TL.encodeUtf8 (TL.fromStrict _ct_api_url)
        config = config0 { configHost = api_endpoint }

    let dispatch = dispatchMime mgr config{configValidateAuthMethods = False}
                    >=> handleMimeError

    oauth_token <- dispatch  $ NBAPI.exchangeApiToken (Accept MimeJSON) (XNeptuneApiToken _ct_token)
    (refresh_thread, oauth_session) <- oauth2_setup (oauth_token ^. neptuneOauthTokenAccessTokenL)
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

teardownNept :: NeptuneSession -> Experiment -> ExperimentState -> Text -> IO ()
teardownNept NeptuneSession{..} experiment state msg = do
    killThread $ experiment ^. exp_transmitter
    killThread $ _neptune_oauth2_refresh

    _neptune_dispatch $ NBAPI.markExperimentCompleted
        (ContentType MimeJSON)
        (Accept MimeNoContent)
        (mkCompletedExperimentParams state msg)
        (experiment ^. exp_experiment_id) :: IO NoContent

    return ()


