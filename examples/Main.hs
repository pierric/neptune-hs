{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
module Main where

import           Neptune
import           RIO

main = do
    withNept "jiasen/sandbox" $ \_ experiment -> do
        forM_ [1..200::Int] $ \i -> do
            nlog experiment "counter" (fromIntegral (i * i) :: Double)
            threadDelay 1000000
