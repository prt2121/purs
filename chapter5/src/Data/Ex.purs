module Ex where

import Prelude
import Data.Array
import Data.Foldable (Foldable, foldl)

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
