module Ex where

import Prelude
import Data.Array
import Data.Foldable (Foldable, foldl)

import Data.Picture (Shape (..), Point (Point))

-- p.56
-- ADT
unitCircle :: Shape
unitCircle = Circle (Point { x: 0.0, y: 0.0 }) 1.0

scale2 :: Shape -> Shape
scale2 (Circle p r) = Circle p (r * 2.0)
scale2 (Rectangle p w h) = Rectangle p (w * 2.0) (h * 2.0)
scale2 (Line start end) = Line start end
scale2 (Text t s) = Text t s

-- Pattern Matching p.48
-- factorial function
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

factorial' :: Int -> Int
factorial' n = foldl (*) 1 (1..n)

type Address = { street :: String, city :: String }
type Person = { name :: String, address :: Address }

livesInLA :: Person -> Boolean
livesInLA { address: { city: "Los Angeles" } } = true
livesInLA _ = false

-- p.51
-- sameCity tests whether two Person records belong to the same city.
sameCity :: Person -> Person -> Boolean
sameCity { address : { city : c1 } } { address : { city : c2 } } = (c1 == c2)
