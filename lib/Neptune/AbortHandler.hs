{-# LANGUAGE DataKinds   #-}
{-# LANGUAGE QuasiQuotes #-}
module Neptune.AbortHandler where

import           Control.Lens          (_Right, (^?!))
import           Control.Retry         (constantDelay, recoverAll)
import           Data.Aeson            (FromJSON (..), Value (..),
                                        eitherDecode', (.:))
import           Data.Aeson.Types      (prependFailure, typeMismatch)
import           Data.Text.Encoding    (encodeUtf8)
import qualified Network.WebSockets    as WS
import           RIO
import           RIO.Text              as T (pack, unpack)
import           Text.URI              (RText, RTextLabel (..), mkURI)
import           Text.URI.Lens
import qualified Wuss

import           Neptune.Backend.Core  (configHost)
import           Neptune.Backend.Model (ExperimentId (..))
import           Neptune.OAuth         (oas_access_token)
import           Neptune.Session       (ClientToken (..), Experiment,
                                        NeptuneSession (..), exp_experiment_id)

abortListener :: NeptuneSession -> Experiment -> ThreadId -> IO ()
abortListener sess exp main_thread = do
    base_url <- mkURI (sess & _neptune_client_token & _ct_api_url)

    let host = base_url ^?! uriAuthority . _Right . authHost . unRText
        path = notif_path <> exp_id <> "/operations" :: String

        oauth_ref = sess & _neptune_oauth2

        run_listener = do
            oauth_current <- readMVar oauth_ref
            let oauth_token = oauth_current ^. oas_access_token
            Wuss.runSecureClientWith
                (T.unpack host) port path
                WS.defaultConnectionOptions
                [("Authorization", "Bearer " <> encodeUtf8 oauth_token)]
                (listener main_thread)

    recoverAll (constantDelay 500) $ \_ -> run_listener

    where
        port       = 443
        notif_path = "/api/notifications/v1/experiments/"
        exp_id     = T.unpack $ unExperimentId $ exp ^. exp_experiment_id


data Message = MessageAbort { _msg_experiment_id :: Text }

instance FromJSON Message where
    parseJSON (Object v) = do
        typ <- v .: "messageType"
        case typ of
          "Abort" -> do
              body <- v .: "messageBody"
              MessageAbort <$> body .: "experimentId"
          _ -> fail $ "Unsupported message type" <> (T.unpack typ)
    parseJSON invalid =
        prependFailure "parsing Message failed, "
                       (typeMismatch "Object" invalid)

data AbortException = AbortException
    deriving Show

instance Exception AbortException

listener :: ThreadId -> WS.ClientApp ()
listener main_thread conn = forever $ do
    msg <- WS.receiveData conn
    case eitherDecode' msg of
      Right (MessageAbort _) -> throwTo main_thread AbortException
      Left msg               -> traceM (T.pack msg) >> return ()

