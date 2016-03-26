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

-- page 69
instance eqNonEmpty :: (Eq a) => Eq (NonEmpty a) where
  eq (NonEmpty x xs) (NonEmpty y ys) = eq x y && eq xs ys

-- > import Prelude
-- > (NonEmpty 1.0 [2.0,3.0]) == (NonEmpty 1.0 [2.0,3.0])
-- true
--
-- > (NonEmpty 1.0 [2.0,3.0]) == (NonEmpty 1.0 [2.0,3.1])
-- false
--
-- > (NonEmpty 1.0 [2.0,3.0]) == (NonEmpty 2.0 [2.0,3.0])
-- false

-- https://github.com/purescript/purescript-prelude/blob/master/src/Data/Semigroup.purs
-- class Semigroup a where
--   append :: a -> a -> a
instance semiGroupNonEmpty :: Semigroup (NonEmpty a) where
  append (NonEmpty x xs) (NonEmpty y ys) = NonEmpty x (xs ++ (y : ys))

  -- > import Prelude
  -- > import Chap6
  -- > append (NonEmpty 1.0 [2.0,3.0]) (NonEmpty 7.0 [9.0])
  -- [ h 1.0, t [2.0,3.0,7.0,9.0]]

-- p72 exercise
-- action respects the concatenation operator of the monoid
class (Monoid m) <= Action m a where
  act :: m -> a -> a

-- the action on arrays is defined by acting on the elements independently
-- instance arrayAction :: (Monoid m, Action m a) => Action m (Array a) where
