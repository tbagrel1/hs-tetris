-- |
-- Module : Utils
-- Description : General helper functions.
-- Copyright : (c) Thomas BAGREL @ TWEAG, 2021
-- License : AGPL-3.0
-- Maintainer : Thomas BAGREL <thomas.bagrel@tweag.io>
-- Stability : experimental
module Utils where

-- | Extract the head of the list if there is some.
headMaybe :: [a] -> Maybe a
headMaybe [] = Nothing
headMaybe (x : xs) = Just x

-- | Extract the last element of the list if there is some.
lastMaybe :: [a] -> Maybe a
lastMaybe [] = Nothing
lastMaybe (x : []) = Just x
lastMaybe (x : xs) = lastMaybe xs

-- | Drops the values from the list whose index does not satisfy the given predicate.
indexFilter ::
  -- | Predicate on index
  (Int -> Bool) ->
  -- | Base list
  [a] ->
  -- | Filtered list
  [a]
indexFilter indexPred xs = fmap snd $ filter (\(i, _) -> indexPred i) $ zip [0 ..] xs
