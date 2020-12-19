{-|
Module      : Neptune.Utils
Description : Neptune Client
Copyright   : (c) Jiasen Wu, 2020
License     : BSD-3-Clause
-}
module Neptune.Utils where

import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Network.HTTP.Client        as NH
import           RIO

import           Neptune.Backend.Client

-- | Unwrap the 'MimeResult a'. Raise an error if it indicates a failure.
handleMimeError :: (Monad m, HasCallStack) => MimeResult a -> m a
handleMimeError result =
    case mimeResult result of
      Left e -> let err_msg = mimeError e
                    response = NH.responseBody $ mimeErrorResponse e
                in error (err_msg ++ " " ++ BSL.unpack response)
      Right r -> return r
