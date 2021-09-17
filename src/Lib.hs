{-# LANGUAGE NamedFieldPuns #-}

module Lib where

import Graphics.Gloss.Interface.IO.Game hiding (Vector)
import Data.Vector (Vector, (!), (//))
import qualified Data.Vector as V
import Data.Ratio ((%))
import System.Random.Shuffle
import Data.Foldable (asum)
import Data.Maybe (isNothing, fromMaybe, isJust)
import Data.List ((\\), groupBy)

(!!!) :: Grid -> GridPoint -> Maybe PieceType
grid !!! (x, y) = grid ! y ! x
infixl 9 !!!  -- same as ! for Data.Vector


rowNb :: Int
rowNb = 20

colNb :: Int
colNb = 10

addPoint :: Num a => (a, a) -> (a, a) -> (a, a)
addPoint (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

type RatPoint = (Rational, Rational)
type GridPoint = (Int, Int)

data MovingPiece = MovingPiece
    { pieceType :: PieceType
    , orientation :: Orientation
    , center :: RatPoint
    }

relGridPoints :: PieceType -> Orientation -> [RatPoint]
relGridPoints pieceType orientation = applyAngle orientation relPositions where
  relPositions = case pieceType of
    I -> [(-3%2, 1%2), (-1%2, 1%2), (1%2, 1%2), (3%2, 1%2)]
    J -> [(-1, 1), (-1, 0), (0, 0), (1, 0)]
    L -> [(-1, 0), (0, 0), (1, 0), (1, 1)]
    O -> [(-1%2, -1%2), (-1%2, 1%2), (1%2, -1%2), (1%2, 1%2)]
    S -> [(-1, 0), (0, 0), (0, 1), (1, 1)]
    T -> [(-1, 0), (0, 0), (0, 1), (1, 0)]
    Z -> [(-1, 1), (0, 1), (0, 0), (1, 0)]

toGridPoint :: RatPoint -> GridPoint
toGridPoint (x, y) = (round x, round y)

isInGrid :: GridPoint -> Bool
isInGrid (x, y) = x >= 0 && x < colNb && y >= 0 && y < rowNb

isInGridOrAbove :: GridPoint -> Bool
isInGridOrAbove (x, y) = x >= 0 && x < colNb && y >= 0

isValid :: Grid -> MovingPiece -> Bool
isValid grid piece = all (\gridPoint -> isInGridOrAbove gridPoint && isNothing (grid !!! gridPoint)) $ gridPoints piece

gridPoints :: MovingPiece -> [GridPoint]
gridPoints MovingPiece { pieceType, orientation, center } = fmap (toGridPoint . addPoint center) (relGridPoints pieceType orientation)

applyAngle :: Orientation -> [RatPoint] -> [RatPoint]
applyAngle orientation = fmap (applyAngle' orientation) where
    applyAngle' orientation (x, y)
        | orientation == S0 = (x, y)
        | orientation == S1 = (y, -x)
        | orientation == S2 = (-x, -y)
        | otherwise = (-y, x)

data Orientation
    = S0
    | S1
    | S2
    | S3
    deriving Eq

data MoveType
    = RotateLeft
    | RotateRight
    | MoveDown
    | MoveLeft
    | MoveRight
    | FullDrop

caseP :: PieceType -> [RatPoint] -> [RatPoint] -> [RatPoint]
caseP pieceType pointsForOthers pointsForI = case pieceType of
  O -> [(0, 0)]
  I -> (0, 0) : pointsForI
  _ -> (0, 0) : pointsForOthers

nextOrientationAndTranslations :: MoveType -> Orientation -> PieceType -> (Orientation, [RatPoint])
nextOrientationAndTranslations moveType orientation pieceType = case (moveType, orientation) of
    (RotateRight, S0) -> (S1, caseP pieceType [(-1, 0), (-1, 1), (0, -2), (-1, -2)] [(-2, 0), (1, 0), (-2, -1), (1, 2)])
    (RotateLeft, S1) -> (S0, caseP pieceType [(1, 0), (1, -1), (0, 2), (1, 2)] [(2, 0), (-1, 0), (2, 1), (-1, -2)])
    (RotateRight, S1) -> (S2, caseP pieceType [(1, 0), (1, -1), (0, 2), (1, 2)] [(-1, 0), (2, 0), (-1, 2), (2, -1)])
    (RotateLeft, S2) -> (S1, caseP pieceType [(-1, 0), (-1, 1), (0, -2), (-1, -2)] [(1, 0), (-2, 0), (1, -2), (-2, 1)])
    (RotateRight, S2) -> (S3, caseP pieceType [(1, 0), (1, 1), (0, -2), (1, -2)] [(2, 0), (-1, 0), (2, 1), (-1, -2)])
    (RotateLeft, S3) -> (S2, caseP pieceType [(-1, 0), (-1, -1), (0, 2), (-1, 2)] [(-2, 0), (1, 0), (-2, -1), (1, 2)])
    (RotateRight, S3) -> (S0, caseP pieceType [(-1, 0), (-1, -1), (0, 2), (-1, 2)] [(1, 0), (-2, 0), (1, -2), (-2, 1)])
    (RotateLeft, S0) -> (S3, caseP pieceType [(1, 0), (1, 1), (0, -2), (1, -2)] [(-1, 0), (2, 0), (-1, 2), (2, -1)])
    (MoveDown, _) -> (orientation, [(0, -1)])
    (MoveLeft, _) -> (orientation, [(-1, 0)])
    (MoveRight, _) -> (orientation, [(1, 0)])
    (FullDrop, _) -> (orientation, fmap (\y -> (0, y)) [(-(toRational rowNb))..0])

filterMaybe :: (a -> Bool) -> [a] -> [Maybe a]
filterMaybe predicate = fmap (\v -> if predicate v then Just v else Nothing)

next :: Grid -> MovingPiece -> MoveType -> Maybe MovingPiece
next grid MovingPiece { pieceType, orientation, center } moveType = asum maybePieces where
    maybePieces = filterMaybe (isValid grid) $ fmap (\trans -> MovingPiece { pieceType, orientation = nextOrientation, center = addPoint center trans }) translations
    (nextOrientation, translations) = nextOrientationAndTranslations moveType orientation pieceType

isDown :: Grid -> MovingPiece -> Bool
isDown grid movingPiece = isNothing $ next grid movingPiece MoveDown

selfRepr :: MovingPiece -> [GridPoint]
selfRepr = filter isInGrid . gridPoints

ghostRepr :: Grid -> MovingPiece -> [GridPoint]
ghostRepr grid movingPiece = (filter isInGrid $ fromMaybe [] $ fmap gridPoints $ next grid movingPiece FullDrop) \\ (selfRepr movingPiece)

data PieceType
    = I
    | J
    | L
    | O
    | S
    | T
    | Z
    deriving Show

type KeyMapping = Key -> Maybe MoveType

defaultKeyMapping :: KeyMapping
defaultKeyMapping key = case key of
    Char 'a' -> Just RotateLeft
    Char 'e' -> Just RotateRight
    Char 's' -> Just MoveDown
    Char 'q' -> Just MoveLeft
    Char 'd' -> Just MoveRight
    Char 'z' -> Just FullDrop
    _ -> Nothing

spawnPiece :: PieceType -> MovingPiece
spawnPiece pieceType = MovingPiece { pieceType, orientation = S0, center } where
    center = case pieceType of
        I -> (toRational rowNb - (1%2), (toInteger colNb - 1)%2)
        O -> (toRational rowNb + (1%2), (toInteger colNb - 1)%2)
        _ -> (toRational rowNb, (toInteger colNb %2) - 1)

pieceTypeFromIndex :: Int -> PieceType
pieceTypeFromIndex i
    | i == 0 = I
    | i == 1 = J
    | i == 2 = L
    | i == 3 = O
    | i == 4 = S
    | i == 5 = T
    | otherwise = Z

generatePieceBag :: IO [PieceType]
generatePieceBag = do
    indices <- shuffleM [0..6]
    return $ fmap pieceTypeFromIndex indices

type Grid = Vector (Vector (Maybe PieceType))

data World = World
    { grid :: Grid
    , movingPiece :: MovingPiece
    , keyMapping :: KeyMapping
    , pieceBag :: [PieceType]
    , linesToDelete :: [Int]
    , dropTickInterval :: Integer
    , tickNo :: Integer
    , tickNextDrop :: Integer
    , finished :: Bool
    }

--initialWorld :: World
--initialWorld = World

handleEvent :: Event -> World -> IO World
handleEvent (EventKey key Down _ _) world@World { grid, movingPiece, keyMapping } = return $ case keyMapping key >>= next grid movingPiece of
    Just newMovingPiece -> world { movingPiece = newMovingPiece }
    Nothing -> world
handleEvent _ world = return world

getFullLines :: Grid -> [Int]
getFullLines grid = fmap fst $ filter snd $ [(y, all (\x -> isJust (grid !!! (x, y))) [0..(colNb-1)]) | y <- [0..(rowNb - 1)]]

indexFilter :: (Int -> Bool) -> [a] -> [a]
indexFilter indexPred xs = fmap snd $ filter (\(i, _) -> indexPred i) $ zip [0..] xs

deleteLines :: Grid -> [Int] -> Grid
deleteLines grid linesToDelete = V.fromList $ filteredLines ++ newLines where
    newLines = replicate n $ V.replicate colNb Nothing
    filteredLines = indexFilter (`notElem` linesToDelete) $ V.toList grid
    n = length linesToDelete

-- check valid only when a piece is just spawned
isFinished :: Grid -> MovingPiece -> Bool
isFinished grid newMovingPiece = any (\gridPoint -> isJust $ grid !!! gridPoint) $ gridPoints newMovingPiece

advanceWorld :: Float -> World -> IO World
advanceWorld _ world = return world

ticksPerSecond :: Int
ticksPerSecond = 30

updateGrid :: Grid -> [(GridPoint, Maybe PieceType)] -> Grid
updateGrid grid updates = grid // newLines where
    newLines = fmap getINewLine groupedUpdates
    groupedUpdates = groupBy (\((_, y1), _) -> \((_, y2), _) -> y1 == y2) updates
    getINewLine lineUpdates = (y, (grid ! y) // xs) where
        y = snd . fst . head $ lineUpdates
        xs = fmap (\((x, _), v) -> (x, v)) lineUpdates

solidifyPiece :: Grid -> MovingPiece -> Grid
solidifyPiece grid movingPiece@MovingPiece { pieceType } = updateGrid grid [((x, y), Just pieceType) | (x, y) <- gridPoints movingPiece]

-- if lines are marked for deletion, delete them, move grid to the bottom, then empty the list
-- if tick is the gravity tick, then move down if possible
-- if down:
--   + solidify the piece in the grid
--   + else:
--       + get a new piece from the bag, and regenerate the bag if necessary
    --   + if end of game:
    --       end of game
--       + increase  interest
--       + check if lines are complete
--         if lines are complete:
--             + mark lines for deletion
--             + increase score
