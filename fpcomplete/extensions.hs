#!/usr/bin/env stack
-- stack --resolver lts-12.21 script
-- Above two lines for scripts, we'll cover that below
{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE ... #-}
{-# OPTIONS_GHC -Wall #-} -- better in package.yaml file, below
module Main (main) where

import Control.Monad (when)
-- import ...

main :: IO ()
main = putStrLn "Finally, some actual code!"