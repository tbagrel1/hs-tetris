{- |
Module : Grid
Description : Row-based grid data structure and associated functions.
Copyright : (c) Thomas BAGREL @ TWEAG, 2021
License : AGPL-3.0
Maintainer : Thomas BAGREL <thomas.bagrel@tweag.io>
Stability : experimental
-}

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

import qualified Data.Vector as V

import Coords
  ( IntCoord,
  )
import Utils
  ( indexFilter,
  )
import Data.Maybe
  ( isJust,
  )
import Data.Vector
  ( Vector,
    (!),
    (//)
  )

{- | Type alias for the row-based grid
-}
type Grid a = Vector (Vector a)

{- | Implements 2D indexing for grid.
Fails if the 2D index is out of bounds.
This operator is defined as 'infixl 9 !!!', like `Data.Vector.(!)
-}
(!!!)
  :: Grid a -- ^ Grid where the value will be fetched
  -> IntCoord -- ^ 2D index (integer coordinates) indicating where to fetch the value in the grid
  -> a -- ^ The resulting value
grid !!! (x, y) = grid ! y ! x
infixl 9 !!!

{- | When the grid is composed of Maybe values, returns the indices of rows which only contain Just values.
-}
fullRows
  :: Grid (Maybe a) -- ^ Grid to work on
  -> [Int] -- ^ indices (0-based) of rows which only contains Just values
fullRows grid = fmap fst $ filter (\(y, xs) -> all isJust xs) $ zip [0 ..] $ V.toList grid

{- | Removes the rows whose indices are given,
push the other rows closer to the bottom of the grid,
and complete the grid with new rows at the top,
created using the specified default value.
-}
deleteRows
  :: Grid a -- ^ Grid to work on
  -> [Int] -- ^ Indices of the rows to delete
  -> a -- ^ Default value which will be replicated to create new rows
  -> Grid a -- ^ Resulting grid
deleteRows grid rowsToDelete defaultValue = V.fromList $ filteredRows ++ newRows
  where
    newRows = replicate n $ V.replicate colNb defaultValue
    filteredRows = indexFilter (`notElem` rowsToDelete) $ V.toList grid
    n = length rowsToDelete
    colNb = V.length $ V.head grid

{- | Updates the given grid using the (2D-index, value) pairs.
-}
update
  :: Grid a -- ^ Grid to work on
  -> [(IntCoord, a)] -- ^ Pairs of (2D-index, value) describing the updates to make
  -> Grid a -- ^ Resulting grid
update = foldl (\g ((x, y), v) -> g // [(y, (g ! y) // [(x, v)])])

{- | Applies the specified function to each cell of the grid.
(I could not implement 'fmap' for 'Grid a' because 'Grid a' is only a type alias)
-}
mapGrid
  :: (a -> b) -- ^ Function to apply on each cell of the grid
  -> Grid a -- ^ Grid to work on
  -> Grid b -- ^ Resulting grid
mapGrid f = fmap (fmap f)

{- | Applies the specified function to each pair (2D-index, value) of the grid,
and collects all the results in a list.
-}
lMapGrid
  :: (IntCoord -> a -> b) -- ^ Function to apply on each (2D-index, value) pair
  -> Grid a -- ^ Grid to work on
  -> [b] -- ^ Resulting list
lMapGrid f grid = do
  (y, row) <- zip [0 ..] $ V.toList grid
  (x, v) <- zip [0 ..] $ V.toList row
  return $ f (x, y) v
