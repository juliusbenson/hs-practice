#!/usr/bin/env stack
-- stack --resolver lts-12.21 script
{-# LANGUAGE BangPatterns #-}

import Conduit
import Control.DeepSeq (force)

average :: Monad m => ConduitM Int o m Double
average =
  divide <$> foldlC add (0, 0)
  where
    divide (total, count) = force fromIntegral total / count
    add (total, count) x = force (total + x, count + 1)

main :: IO ()
main = print $ runConduitPure $ enumFromToC 1 1000000 .| average