module MyMath (answer, circleArea) where

import Prelude
import Math (sqrt, pi)

diagonal :: Number -> Number -> Number
diagonal w h = sqrt (w * w + h * h)

answer :: Number
answer = diagonal 3.0 4.0

circleArea :: Number -> Number
circleArea r = pi * r * r
