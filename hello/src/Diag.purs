module Diag (answer) where

import Prelude
import Math (sqrt)

diagonal :: Number -> Number -> Number
diagonal w h = sqrt (w * w + h * h)

answer :: Number
answer = diagonal 3.0 4.0
