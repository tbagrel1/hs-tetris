{-# LANGUAGE NamedFieldPuns #-}

-- |
-- Module : Pieces
-- Description : Type and functions related to Tetris pieces (Tetrominos)
-- Copyright : (c) Thomas BAGREL @ TWEAG, 2021
-- License : AGPL-3.0
-- Maintainer : Thomas BAGREL <thomas.bagrel@tweag.io>
-- Stability : experimental
module Pieces
  ( Orientation (..),
    PieceType (..),
    MovingPiece (..),
    PieceGrid,
    generatePieceBag,
    spawnPiece,
    applyMove,
    solidifyPiece,
    selfRepr,
    ghostRepr,
    isValid,
  )
where

import Coords
  ( IntCoord,
    RatCoord,
    addCoord,
    toIntCoord,
  )
import Data.Maybe
  ( fromMaybe,
    isNothing,
  )
import Data.Ratio
  ( (%),
  )
import Game
  ( MoveType (..),
    colNb,
    isInGridOrAbove,
    isInTotalGrid,
    playableRowNb,
    totalRowNb,
  )
import Grid
  ( Grid,
    update,
    (!!!),
  )
import System.Random.Shuffle
  ( shuffleM,
  )
import Utils
  ( headMaybe,
    lastMaybe,
  )

-- | Type alias for a grid in which each cell can contain a colored square or be empty.
type PieceGrid = Grid (Maybe PieceType)

-- | Represents the four orientations a piece can take in Tetris
data Orientation
  = -- | Initial state, horizontal (for most pieces)
    S0
  | -- | After 1 clockwise rotation action
    S1
  | -- | After 2 rotation actions
    S2
  | -- | After 3 clockwise rotation actions
    S3
  deriving (Eq, Show)

-- | Tetrominos types
data PieceType
  = I
  | J
  | L
  | O
  | S
  | T
  | Z
  deriving (Eq, Show)

-- | The moving piece (i.e. one which is still falling) is defined by its type, orientation and center of rotation
data MovingPiece = MovingPiece
  { -- | Type of tetromino
    pieceType :: PieceType,
    -- | Current orientation of the piece
    orientation :: Orientation,
    -- | Center of rotation of this piece. Can be a rational point (we consider that the center of each grid cell has integer coordinates, so the borders/corners of cells have rational coordinates)
    center :: RatCoord
  }
  deriving (Eq, Show)

-- | Gives the relative positions of a tetromino squares with relation to its center and orientation.
pieceRelCoords ::
  -- | Type of tetromino
  PieceType ->
  -- | Current orientation of this tetromino
  Orientation ->
  -- | List of relative square positions
  [RatCoord]
pieceRelCoords pieceType orientation = fmap (applyOrientation orientation) relPositionsS0
  where
    relPositionsS0 = case pieceType of
      I -> [(-3 % 2, 1 % 2), (-1 % 2, 1 % 2), (1 % 2, 1 % 2), (3 % 2, 1 % 2)]
      J -> [(-1, 1), (-1, 0), (0, 0), (1, 0)]
      L -> [(-1, 0), (0, 0), (1, 0), (1, 1)]
      O -> [(-1 % 2, -1 % 2), (-1 % 2, 1 % 2), (1 % 2, -1 % 2), (1 % 2, 1 % 2)]
      S -> [(-1, 0), (0, 0), (0, 1), (1, 1)]
      T -> [(-1, 0), (0, 0), (0, 1), (1, 0)]
      Z -> [(-1, 1), (0, 1), (0, 0), (1, 0)]

-- | Gives the absolute positions of a tetromino squares in the grid.
pieceAbsCoords ::
  -- | Represents the tetromino (because we need all three fields of 'MovingPiece' to compute the absolute positions)
  MovingPiece ->
  -- | List of absolute square positions (grid coordinates)
  [IntCoord]
pieceAbsCoords MovingPiece {pieceType, orientation, center} = fmap (toIntCoord . addCoord center) (pieceRelCoords pieceType orientation)

-- | Tells whether a moving piece position is valid or not:
--    + all its squares must be in the grid or its vertical extension
--    + the squares must not overlap an already solidified colored square in the grid
isValid ::
  -- | The grid to work on
  PieceGrid ->
  -- | The piece position to check
  MovingPiece ->
  -- | True iif the moving piece position is valid
  Bool
isValid grid piece = all (\gridPoint -> isInGridOrAbove gridPoint && isNothing (grid !!! gridPoint)) $ pieceAbsCoords piece

-- | Rotates a relative position given for S0 to the one for the specified orientation.
applyOrientation ::
  -- | Output orientation
  Orientation ->
  -- | Relative position for S0
  RatCoord ->
  -- | Relative position for the specified orientation
  RatCoord
applyOrientation orientation (x, y) = case orientation of
  S0 -> (x, y)
  S1 -> (y, -x)
  S2 -> (-x, -y)
  S3 -> (-y, x)

-- | Computes the next orientation of a piece and the list of translations to try when an action is requested by the user.
nextOrientationAndTranslations ::
  -- | Type of action
  MoveType ->
  -- | Current orientation of the tetromino
  Orientation ->
  -- | Type of tetromino
  PieceType ->
  -- | Future orientation and list of translations to try
  (Orientation, [RatCoord])
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
  (FullDrop, _) -> (orientation, [(0, y) | y <- [0, -1 .. (-(toRational totalRowNb))]])
  where
    originThenCase pointsForOthers pointsForI = case pieceType of
      O -> [(0, 0)]
      I -> (0, 0) : pointsForI
      _ -> (0, 0) : pointsForOthers

-- | Given the grid, the current moving piece and the move type requested by the user,
-- returns a moving piece with its orientation and position updated, if the move is possible.
applyMove ::
  -- | The grid to work on. Used to check which moves are valid or not
  PieceGrid ->
  -- | The current state of the falling piece
  MovingPiece ->
  -- | Move requested by the user
  MoveType ->
  -- | Updated moving piece if the move can be executed
  Maybe MovingPiece
applyMove grid MovingPiece {pieceType, orientation, center} moveType =
  if moveType == FullDrop
    then lastMaybe $ takeWhile (isValid grid) potentialMovingPieces
    else headMaybe $ filter (isValid grid) potentialMovingPieces
  where
    potentialMovingPieces = fmap (\trans -> MovingPiece {pieceType, orientation = nextOrientation, center = addCoord center trans}) translations
    (nextOrientation, translations) = nextOrientationAndTranslations moveType orientation pieceType

-- | When a moving piece has finished falling, this function transforms it into colored square cells in the grid.
solidifyPiece ::
  -- | The grid to update
  PieceGrid ->
  -- | The moving piece to solidify
  MovingPiece ->
  -- | The updated grid
  PieceGrid
solidifyPiece grid movingPiece@MovingPiece {pieceType} = update grid [((x, y), Just pieceType) | (x, y) <- pieceAbsCoords movingPiece]

-- | Returns the grid cells on which the moving piece must be shown.
selfRepr ::
  -- | The current moving piece
  MovingPiece ->
  -- | List of grid coordinates on which to show the piece
  [IntCoord]
selfRepr = filter isInTotalGrid . pieceAbsCoords

-- | Returns the grid cells on which the "ghost" (i.e. the ground projection) of the moving piece must be shown.
ghostRepr ::
  -- | The grid
  PieceGrid ->
  -- | The current moving piece
  MovingPiece ->
  -- | List of grid coordinates on which to show the piece "ghost"
  [IntCoord]
ghostRepr grid movingPiece = filter isInTotalGrid $ maybe [] pieceAbsCoords $ applyMove grid movingPiece FullDrop

-- | Creates a new moving piece using the specified type.
spawnPiece ::
  -- | Type of tetromino to spawn
  PieceType ->
  -- | The resulting moving piece
  MovingPiece
spawnPiece pieceType = MovingPiece {pieceType, orientation = S0, center}
  where
    center = case pieceType of
      I -> ((toInteger colNb - 1) % 2, toRational playableRowNb - (1 % 2))
      O -> ((toInteger colNb - 1) % 2, toRational playableRowNb + (1 % 2))
      _ -> ((toInteger colNb % 2) - 1, toRational playableRowNb)

-- | Returns a different piece type for each integer in range [0..6].
pieceTypeFromIndex :: Int -> PieceType
pieceTypeFromIndex i = case i of
  0 -> I
  1 -> J
  2 -> L
  3 -> O
  4 -> S
  5 -> T
  _ -> Z

-- | Generates a list of piece types where each type appears only one (but in a random order).
generatePieceBag :: IO [PieceType]
generatePieceBag = do
  indices <- shuffleM [0 .. 6]
  return $ fmap pieceTypeFromIndex indices
