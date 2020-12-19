{-|
Module      : Neptune
Description : Neptune Client
Copyright   : (c) Jiasen Wu, 2020
License     : BSD-3-Clause

This is a client library for <https://neptune.ai>

=== Example:

@
main = do
    -- Experiment 'sandbox' must be created from the Neptune dashboard
    withNept "jiasen/sandbox" $ \session experiment -> do
        forM_ [1..10::Int] $ \i -> do
            -- You can log a name-value pair.
            nlog experiment "counter" (fromIntegral (i * i) :: Double)
            threadDelay 1000000
@
-}

module Neptune
  ( module Neptune.Client
  , module Neptune.Session
  ) where

import           Neptune.Client
import           Neptune.Session
