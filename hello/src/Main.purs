module Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, print)

import Diag (answer)

main :: forall e. Eff (console :: CONSOLE | e) Unit
main = print answer
