module Main where

import Rattus
import Rattus.Stream

{-# ANN module Rattus #-}

main :: IO ()
main = putStrLn "Testing loopIndirect"

incompatibleAdv :: O v Int -> O v Int -> O v Int
incompatibleAdv li lk = delay (adv li + adv lk)
