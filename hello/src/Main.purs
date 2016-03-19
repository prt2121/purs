module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, print)

import MyMath (answer, circleArea)

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  print answer
  print $ circleArea 1.0
