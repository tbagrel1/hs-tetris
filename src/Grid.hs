module Grid
    ( Grid
    , (!!!)
    , fullLines
    , deleteLines
    , update
    , mapGrid
    , lMapGrid
    ) where

import Coords
import Utils (indexFilter)

import qualified Data.Vector as V
import Data.Vector (Vector, (!), (//))
import Data.Maybe (isJust)

type Grid a = Vector (Vector a)

(!!!) :: Grid a -> IntCoord -> a
grid !!! (x, y) = grid ! y ! x
infixl 9 !!!  -- same as ! for Data.Vector

fullLines :: Grid (Maybe a) -> [Int]
fullLines grid = fmap fst $ filter (\(y, xs) -> all isJust xs) $ zip [0..] $ V.toList grid

deleteLines :: Grid a -> [Int] -> a -> Grid a
deleteLines grid linesToDelete defaultValue = V.fromList $ filteredLines ++ newLines where
    newLines = replicate n $ V.replicate colNb defaultValue
    filteredLines = indexFilter (`notElem` linesToDelete) $ V.toList grid
    n = length linesToDelete
    colNb = V.length $ V.head grid

update :: Grid a -> [(IntCoord, a)] -> Grid a
update grid updates = foldl (\g -> \((x, y), v) -> g // [(y, (g ! y) // [(x, v)])]) grid updates

mapGrid :: (a -> b) -> Grid a -> Grid b
mapGrid f = fmap (fmap f)

lMapGrid :: (IntCoord -> a -> b) -> Grid a -> [b]
lMapGrid f grid = do
        (y, line) <- zip [0..] $ V.toList grid
        (x, v) <- zip [0..] $ V.toList line
        return $ f (x, y) v
