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
import Control.Monad ((>=>), guard)

rowNb :: Int
rowNb = 20

colNb :: Int
colNb = 10

ticksPerSecond :: Int
ticksPerSecond = 30

initialDropTickInterval :: Int
initialDropTickInterval = 10

fullLineColor :: Color
fullLineColor = makeColorI 219 193 195 255

backgroundColor :: Color
backgroundColor = white

type RatPoint = (Rational, Rational)
type GridPoint = (Int, Int)
type KeyMapping = Key -> Maybe MoveType
type Grid = Vector (Vector (Maybe PieceType))
type ColorGrid = Vector (Vector (Color))

data Orientation
    = S0
    | S1
    | S2
    | S3
    deriving (Eq, Show)

data PieceType
    = I
    | J
    | L
    | O
    | S
    | T
    | Z
    deriving (Eq, Show)

data MoveType
    = RotateLeft
    | RotateRight
    | MoveDown
    | MoveLeft
    | MoveRight
    | FullDrop
    deriving (Eq, Show)

data MovingPiece = MovingPiece
    { pieceType :: PieceType
    , orientation :: Orientation
    , center :: RatPoint
    } deriving (Eq, Show)

data World = World
    { grid :: Grid
    , movingPiece :: MovingPiece
    , pieceBag :: [PieceType]
    , linesToDelete :: [Int]
    , dropTickInterval :: Integer
    , tickNo :: Integer
    , tickNextDrop :: Integer
    , finished :: Bool
    , linesCleared :: Int
    } deriving (Eq, Show)

(!!!) :: Vector (Vector a) -> GridPoint -> a
grid !!! (x, y) = grid ! y ! x
infixl 9 !!!  -- same as ! for Data.Vector

addPoint :: Num a => (a, a) -> (a, a) -> (a, a)
addPoint (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

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
ghostRepr grid movingPiece = filter isInGrid $ fromMaybe [] $ fmap gridPoints $ next grid movingPiece FullDrop

keyMapping :: KeyMapping
keyMapping key = case key of
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
        I -> ((toInteger colNb - 1)%2, toRational rowNb - (1%2))
        O -> ((toInteger colNb - 1)%2, toRational rowNb + (1%2))
        _ -> ((toInteger colNb %2) - 1, toRational rowNb)

pieceTypeFromIndex :: Int -> PieceType
pieceTypeFromIndex i
    | i == 0 = I
    | i == 1 = J
    | i == 2 = L
    | i == 3 = O
    | i == 4 = S
    | i == 5 = T
    | otherwise = Z

colorFromPieceType :: PieceType -> Color
colorFromPieceType p
    | p == I = makeColorI 23 205 255 255
    | p == J = makeColorI 30 49 148 255
    | p == L = makeColorI 255 128 0 255
    | p == O = makeColorI 255 217 0 255
    | p == S = makeColorI 12 173 42 255
    | p == T = makeColorI 138 25 138 255
    | otherwise = makeColorI 191 23 37 255

ghostColor :: Color -> Color
ghostColor = bright

generatePieceBag :: IO [PieceType]
generatePieceBag = do
    indices <- shuffleM [0..6]
    return $ fmap pieceTypeFromIndex indices

handleEvent :: Event -> World -> IO World
handleEvent (EventKey key Down _ _) world@World { grid, movingPiece } = return $ case keyMapping key >>= next grid movingPiece of
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

-- check can only be true when a piece is just spawned
isFinished :: Grid -> MovingPiece -> Bool
isFinished grid newMovingPiece = any (\gridPoint -> isJust $ grid !!! gridPoint) $ gridPoints newMovingPiece

updateGrid :: (Vector (Vector a)) -> [(GridPoint, a)] -> (Vector (Vector a))
updateGrid grid updates = grid // newLines where
    newLines = fmap getINewLine groupedUpdates
    groupedUpdates = groupBy (\((_, y1), _) -> \((_, y2), _) -> y1 == y2) updates
    getINewLine lineUpdates = (y, (grid ! y) // xs) where
        y = snd . fst . head $ lineUpdates
        xs = fmap (\((x, _), v) -> (x, v)) lineUpdates

solidifyPiece :: Grid -> MovingPiece -> Grid
solidifyPiece grid movingPiece@MovingPiece { pieceType } = updateGrid grid [((x, y), Just pieceType) | (x, y) <- gridPoints movingPiece]

makeInitialWorld :: IO World
makeInitialWorld = do
    (firstPieceType : pieceBag) <- generatePieceBag
    return World
        { grid = V.replicate (rowNb + 2) $ V.replicate colNb Nothing
        , movingPiece = spawnPiece firstPieceType
        , pieceBag = pieceBag
        , linesToDelete = []
        , dropTickInterval = toInteger initialDropTickInterval
        , tickNo = 0
        , tickNextDrop = toInteger initialDropTickInterval
        , finished = False
        , linesCleared = 0
        }

advanceWorld :: Float -> World -> IO World
advanceWorld _ world@World { movingPiece = movingPiece@MovingPiece { center }} = do
    putStrLn "advancing..."
    putStrLn $ show center
    (deleteMarkedLines >=> applyGravity >=> ifDownActions) world

-- 1. if lines are marked for deletion, delete them, move grid to the bottom, then empty the list
deleteMarkedLines :: World -> IO World
deleteMarkedLines world@World { grid, linesToDelete } =
    return world
        { grid = if null linesToDelete then grid else deleteLines grid linesToDelete
        , linesToDelete = []
        }

-- 2. if tick is the gravity tick, then move down if possible
applyGravity :: World -> IO World
applyGravity world@World { grid, dropTickInterval, tickNo, tickNextDrop, movingPiece } =
    let tickNo' = tickNo + 1
        (movingPiece', tickNextDrop') =
            if tickNo' >= tickNextDrop then
                (fromMaybe movingPiece $ next grid movingPiece MoveDown, tickNextDrop + dropTickInterval)
            else
                (movingPiece, tickNextDrop) in
    return world
        { movingPiece = movingPiece'
        , tickNo = tickNo'
        , tickNextDrop = tickNextDrop'
        }

-- 3. if down:
ifDownActions :: World -> IO World
ifDownActions world@World { grid, movingPiece } =
    if isDown grid movingPiece then do
        putStrLn "Down"
        solidifyAndNewMovingPiece >=> updateFinished >=> updateLinesToDelete $ world
    else return world

--   + 3.1 solidify the piece in the grid
--         and get a new piece from the bag, and regenerate the bag if necessary
solidifyAndNewMovingPiece :: World -> IO World
solidifyAndNewMovingPiece world@World { grid, pieceBag, movingPiece } = do
    let grid' = solidifyPiece grid movingPiece
    (newPieceType : pieceBag') <- if null pieceBag then generatePieceBag else return pieceBag
    let movingPiece' = spawnPiece newPieceType
    return world
        { grid = grid'
        , movingPiece = movingPiece'
        , pieceBag = pieceBag'
        }

--   + 3.2 check end of game:
updateFinished :: World -> IO World
updateFinished world@World { grid, movingPiece, finished } = do
    putStrLn $ "finished: " ++ show world
    return world { finished = finished || isFinished grid movingPiece }

--   + 3.3 check if lines are complete
--         if lines are complete:
--             + mark lines for deletion
--             + increase score
updateLinesToDelete :: World -> IO World
updateLinesToDelete world@World { grid, linesCleared } =
    let fullLines = getFullLines grid
        n = length fullLines in
    return world
        { linesToDelete = getFullLines grid
        , linesCleared = linesCleared + n
        }

mapGrid :: (a -> b) -> Vector (Vector a) -> Vector (Vector b)
mapGrid f = fmap (fmap f)

lmapGrid :: (GridPoint -> a -> b) -> Vector (Vector a) -> [b]
lmapGrid f grid = do
        (y, line) <- zip [0..] $ V.toList grid
        (x, v) <- zip [0..] $ V.toList line
        return $ f (x, y) v

getColorGrid :: World -> ColorGrid
getColorGrid world@World { grid, movingPiece = movingPiece@MovingPiece { pieceType }, linesToDelete } =
    (solidifiedColorGrid `updateGrid` linesToDeleteUpdate) `updateGrid` ghostUpdate `updateGrid` movingUpdate where
        solidifiedColorGrid = mapGrid (fromMaybe backgroundColor . fmap colorFromPieceType) grid
        linesToDeleteUpdate = [((x, y), fullLineColor) | x <- [0..(colNb - 1)], y <- linesToDelete]
        ghostUpdate = [((x, y), ghostColor $ colorFromPieceType pieceType) | (x, y) <- ghostRepr grid movingPiece]
        movingUpdate = [((x, y), colorFromPieceType pieceType) | (x, y) <- selfRepr movingPiece]
