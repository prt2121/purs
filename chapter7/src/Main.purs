module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Apply
import Data.Maybe
import Math

maybePlus :: forall a. (Semiring a) => Maybe a -> Maybe a -> Maybe a
maybePlus = lift2 (+)

-- import Control.Apply
-- import Prelude
-- import Data.Maybe
-- liftedPlus :: Maybe Int
-- liftedPlus = lift2 (+) 5 2
-- liftedPlus x y = lift2 (+) x y
-- Just (5)

maybeMinus :: forall a. (Ring a) => Maybe a -> Maybe a -> Maybe a
maybeMinus = lift2 (-)

maybeMul :: forall a. (Semiring a) => Maybe a -> Maybe a -> Maybe a
maybeMul = lift2 (*)

maybeDiv :: forall a. (ModuloSemiring a) => Maybe a -> Maybe a -> Maybe a
maybeDiv = lift2 (/)

-- forall a f. (Applicativef) => Maybe (f a) -> f (Maybe a)


main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  log "Hello sailor!"

  -- import Data.Maybe
  -- liftedPlus (Just 2) (Just 3)
  -- Just (5)
