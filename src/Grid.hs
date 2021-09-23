-- |
-- Module : Grid
-- Description : Row-based grid data structure and associated functions.
-- Copyright : (c) Thomas BAGREL @ TWEAG, 2021
-- License : AGPL-3.0
-- Maintainer : Thomas BAGREL <thomas.bagrel@tweag.io>
-- Stability : experimental
module Grid
  ( Grid,
    (!!!),
    fullRows,
    deleteRows,
    update,
    mapGrid,
    lMapGrid,
  )
where

import Coords
  ( IntCoord,
  )
import Data.Maybe
  ( isJust,
  )
import Data.Vector
  ( Vector,
    (!),
    (//),
  )
import qualified Data.Vector as V
import Utils
  ( indexFilter,
  )

-- | Type alias for the row-based grid
type Grid a = Vector (Vector a)

-- | Implements 2D indexing for grid.
-- Fails if the 2D index is out of bounds.
(!!!) ::
  -- | Grid where the value will be fetched
  Grid a ->
  -- | 2D index (integer coordinates) indicating where to fetch the value in the grid
  IntCoord ->
  -- | The resulting value
  a
grid !!! (x, y) = grid ! y ! x

infixl 9 !!!

-- | When the grid is composed of Maybe values, returns the indices of rows which only contain Just values.
fullRows ::
  -- | Grid to work on
  Grid (Maybe a) ->
  -- | indices (0-based) of rows which only contains Just values
  [Int]
fullRows grid = fmap fst $ filter (\(y, xs) -> all isJust xs) $ zip [0 ..] $ V.toList grid

-- | Removes the rows whose indices are given,
-- push the other rows closer to the bottom of the grid,
-- and complete the grid with new rows at the top,
-- created using the specified default value.
deleteRows ::
  -- | Grid to work on
  Grid a ->
  -- | Indices of the rows to delete
  [Int] ->
  -- | Default value which will be replicated to create new rows
  a ->
  -- | Resulting grid
  Grid a
deleteRows grid rowsToDelete defaultValue = V.fromList $ filteredRows ++ newRows
  where
    newRows = replicate n $ V.replicate colNb defaultValue
    filteredRows = indexFilter (`notElem` rowsToDelete) $ V.toList grid
    n = length rowsToDelete
    colNb = V.length $ V.head grid

-- | Updates the given grid using the (2D-index, value) pairs.
update ::
  -- | Grid to work on
  Grid a ->
  -- | Pairs of (2D-index, value) describing the updates to make
  [(IntCoord, a)] ->
  -- | Resulting grid
  Grid a
update = foldl (\g ((x, y), v) -> g // [(y, (g ! y) // [(x, v)])])

-- | Applies the specified function to each cell of the grid.
-- (I could not implement 'fmap' for 'Grid a' because 'Grid a' is only a type alias)
mapGrid ::
  -- | Function to apply on each cell of the grid
  (a -> b) ->
  -- | Grid to work on
  Grid a ->
  -- | Resulting grid
  Grid b
mapGrid f = fmap (fmap f)

-- | Applies the specified function to each pair (2D-index, value) of the grid,
-- and collects all the results in a list.
lMapGrid ::
  -- | Function to apply on each (2D-index, value) pair
  (IntCoord -> a -> b) ->
  -- | Grid to work on
  Grid a ->
  -- | Resulting list
  [b]
lMapGrid f grid = do
  (y, row) <- zip [0 ..] $ V.toList grid
  (x, v) <- zip [0 ..] $ V.toList row
  return $ f (x, y) v
