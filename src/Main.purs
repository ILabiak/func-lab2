module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Lab2 as Lab2

main :: Effect Unit
main = do
  Lab2.test
