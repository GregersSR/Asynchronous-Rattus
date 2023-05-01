#!/usr/bin/env cabal
{- cabal:
build-depends: Rattus, base
-}

module Main where

import Rattus
import Rattus.Stream
import Rattus.Plugin.Annotation (InternalAnn(..))

{-# ANN module Rattus #-}

loopIndirect :: Str v Int
loopIndirect = run
  where run :: Str v Int
        run = loopIndirect

main :: IO ()
main = putStrLn "Testing loopIndirect"