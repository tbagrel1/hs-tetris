{- |
Module : Coords
Description : Type aliases and functions to deal with coordinates (either integer or rational coordinates)
Copyright : (c) Thomas BAGREL @ TWEAG, 2021
License : AGPL-3.0
Maintainer : Thomas BAGREL <thomas.bagrel@tweag.io>
Stability : experimental
-}

module Coords
  ( RatCoord,
    IntCoord,
    addCoord,
    toIntCoord,
  )
where

{- | Type alias for rational coordinates
-}
type RatCoord = (Rational, Rational)

{- | Type alias for integer coordinates
-}
type IntCoord = (Int, Int)

{- | Translates the first point using the second one as a movement vector.
-}
addCoord
  :: Num a
  => (a, a) -- ^ First point
  -> (a, a) -- ^ Second point
  -> (a, a) -- ^ Result of the translation
addCoord (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

{- | Rounds the point coordinates to the nearest integers to give a IntCoord
-}
toIntCoord
  :: RatCoord -- ^ Rational coordinates to round
  -> IntCoord -- ^ Result of the rounding
toIntCoord (x, y) = (round x, round y)
