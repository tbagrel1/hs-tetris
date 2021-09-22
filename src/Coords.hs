module Coords
    ( RatCoord
    , IntCoord
    , addCoord
    , toIntCoord
    ) where

type RatCoord = (Rational, Rational)
type IntCoord = (Int, Int)

addCoord :: Num a => (a, a) -> (a, a) -> (a, a)
addCoord (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

toIntCoord :: RatCoord -> IntCoord
toIntCoord (x, y) = (round x, round y)
