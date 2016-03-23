module Chap6 where

import Prelude
import Data.Array
import Data.Monoid
import Data.Foldable

newtype Complex = Complex
  { real :: Number
  , imaginary :: Number
  }

-- Define Show and Eq instances for Complex.
instance showComplex :: Show Complex where
  show (Complex { real = r, imaginary = i }) =
    "Complex (r:" ++ show r ++ ", i:" ++ show i ++ ")"

-- type of non-empty arrays of elements of type a
data NonEmpty a = NonEmpty a (Array a)

instance showNonEmpty :: (Show a) => Show (NonEmpty a) where
  show (NonEmpty x xs) = "[ h " ++ show x ++ ", t " ++ show xs ++ "]"

-- https://github.com/purescript/purescript-prelude/blob/master/src/Data/Semigroup.purs
-- class Semigroup a where
--   append :: a -> a -> a
instance semiGroupNonEmpty :: Semigroup (NonEmpty a) where
  append (NonEmpty x xs) (NonEmpty y ys) = NonEmpty x (xs ++ (y : ys))

  -- > import Prelude
  -- > import Chap6
  -- > append (NonEmpty 1.0 [2.0,3.0]) (NonEmpty 7.0 [9.0])
  -- [ h 1.0, t [2.0,3.0,7.0,9.0]]
