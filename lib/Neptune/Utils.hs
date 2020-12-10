{-# LANGUAGE ViewPatterns #-}
module Neptune.Utils where

import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Network.HTTP.Client        as NH
import           RIO

import           Neptune.Backend.Client

handleMimeError :: (Monad m, HasCallStack) => MimeResult a -> m a
handleMimeError (mimeResult -> Left e)  = let err_msg = mimeError e
                                              response = NH.responseBody $ mimeErrorResponse e
                                           in error (mimeError e ++ " " ++ BSL.unpack response)
handleMimeError (mimeResult -> Right r) = return r
