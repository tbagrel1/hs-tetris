{-# LANGUAGE NamedFieldPuns #-}

module Pieces
    ( Orientation (..)
    , PieceType (..)
    , MovingPiece (..)
    , PieceGrid
    , generatePieceBag
    , spawnPiece
    , applyMove
    , solidifyPiece
    , selfRepr
    , ghostRepr
    ) where

import Grid
import Coords
import Game
import Data.Ratio ((%))
import Data.Maybe (isNothing, fromMaybe)
import Utils (lastMaybe, headMaybe)
import System.Random.Shuffle (shuffleM)

type PieceGrid = Grid (Maybe PieceType)

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

data MovingPiece = MovingPiece
    { pieceType :: PieceType
    , orientation :: Orientation
    , center :: RatCoord
    } deriving (Eq, Show)

pieceRelCoords :: PieceType -> Orientation -> [RatCoord]
pieceRelCoords pieceType orientation = fmap (applyOrientation orientation) relPositions where
  relPositions = case pieceType of
    I -> [(-3%2, 1%2), (-1%2, 1%2), (1%2, 1%2), (3%2, 1%2)]
    J -> [(-1, 1), (-1, 0), (0, 0), (1, 0)]
    L -> [(-1, 0), (0, 0), (1, 0), (1, 1)]
    O -> [(-1%2, -1%2), (-1%2, 1%2), (1%2, -1%2), (1%2, 1%2)]
    S -> [(-1, 0), (0, 0), (0, 1), (1, 1)]
    T -> [(-1, 0), (0, 0), (0, 1), (1, 0)]
    Z -> [(-1, 1), (0, 1), (0, 0), (1, 0)]

pieceAbsCoords :: MovingPiece -> [IntCoord]
pieceAbsCoords MovingPiece { pieceType, orientation, center } = fmap (toIntCoord . addCoord center) (pieceRelCoords pieceType orientation)

isInGrid :: IntCoord -> Bool
isInGrid (x, y) = x >= 0 && x < colNb && y >= 0 && y < (rowNb + 2)

isInGridOrAbove :: IntCoord -> Bool
isInGridOrAbove (x, y) = x >= 0 && x < colNb && y >= 0

isValid :: PieceGrid -> MovingPiece -> Bool
isValid grid piece = all (\gridPoint -> isInGridOrAbove gridPoint && isNothing (grid !!! gridPoint)) $ pieceAbsCoords piece

applyOrientation :: Orientation -> RatCoord -> RatCoord
applyOrientation orientation (x, y)
    | orientation == S0 = (x, y)
    | orientation == S1 = (y, -x)
    | orientation == S2 = (-x, -y)
    | otherwise = (-y, x)

nextOrientationAndTranslations :: MoveType -> Orientation -> PieceType -> (Orientation, [RatCoord])
nextOrientationAndTranslations moveType orientation pieceType = case (moveType, orientation) of
    (RotateRight, S0) -> (S1, originThenCase [(-1, 0), (-1, 1), (0, -2), (-1, -2)] [(-2, 0), (1, 0), (-2, -1), (1, 2)])
    (RotateLeft, S1) -> (S0, originThenCase [(1, 0), (1, -1), (0, 2), (1, 2)] [(2, 0), (-1, 0), (2, 1), (-1, -2)])
    (RotateRight, S1) -> (S2, originThenCase [(1, 0), (1, -1), (0, 2), (1, 2)] [(-1, 0), (2, 0), (-1, 2), (2, -1)])
    (RotateLeft, S2) -> (S1, originThenCase [(-1, 0), (-1, 1), (0, -2), (-1, -2)] [(1, 0), (-2, 0), (1, -2), (-2, 1)])
    (RotateRight, S2) -> (S3, originThenCase [(1, 0), (1, 1), (0, -2), (1, -2)] [(2, 0), (-1, 0), (2, 1), (-1, -2)])
    (RotateLeft, S3) -> (S2, originThenCase [(-1, 0), (-1, -1), (0, 2), (-1, 2)] [(-2, 0), (1, 0), (-2, -1), (1, 2)])
    (RotateRight, S3) -> (S0, originThenCase [(-1, 0), (-1, -1), (0, 2), (-1, 2)] [(1, 0), (-2, 0), (1, -2), (-2, 1)])
    (RotateLeft, S0) -> (S3, originThenCase [(1, 0), (1, 1), (0, -2), (1, -2)] [(-1, 0), (2, 0), (-1, 2), (2, -1)])
    (MoveDown, _) -> (orientation, [(0, -1)])
    (MoveLeft, _) -> (orientation, [(-1, 0)])
    (MoveRight, _) -> (orientation, [(1, 0)])
    (FullDrop, _) -> (orientation, [(0, y) | y <- [0,-1..(-(toRational rowNb + 2))]])
    where
        originThenCase pointsForOthers pointsForI = case pieceType of
          O -> [(0, 0)]
          I -> (0, 0) : pointsForI
          _ -> (0, 0) : pointsForOthers

applyMove :: PieceGrid -> MovingPiece -> MoveType -> Maybe MovingPiece
applyMove grid MovingPiece { pieceType, orientation, center } moveType =
    if moveType == FullDrop then
       lastMaybe $ takeWhile (isValid grid) potentialMovingPieces
    else
       headMaybe $ filter (isValid grid) potentialMovingPieces
       where
            potentialMovingPieces = fmap (\trans -> MovingPiece { pieceType, orientation = nextOrientation, center = addCoord center trans }) translations
            (nextOrientation, translations) = nextOrientationAndTranslations moveType orientation pieceType

solidifyPiece :: PieceGrid -> MovingPiece -> PieceGrid
solidifyPiece grid movingPiece@MovingPiece { pieceType } = update grid [((x, y), Just pieceType) | (x, y) <- pieceAbsCoords movingPiece]

selfRepr :: MovingPiece -> [IntCoord]
selfRepr = filter isInGrid . pieceAbsCoords

ghostRepr :: PieceGrid -> MovingPiece -> [IntCoord]
ghostRepr grid movingPiece = filter isInGrid $ fromMaybe [] $ fmap pieceAbsCoords $ applyMove grid movingPiece FullDrop

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

generatePieceBag :: IO [PieceType]
generatePieceBag = do
    indices <- shuffleM [0..6]
    return $ fmap pieceTypeFromIndex indices
