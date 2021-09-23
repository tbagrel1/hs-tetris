{-# LANGUAGE NamedFieldPuns #-}

-- |
-- Module : Render
-- Description : Rendering functions for TETRIS
-- Copyright : (c) Thomas BAGREL @ TWEAG, 2021
-- License : AGPL-3.0
-- Maintainer : Thomas BAGREL <thomas.bagrel@tweag.io>
-- Stability : experimental
module Render
  ( backgroundColor,
    gridWidth,
    gridHeight,
    withMargin,
    renderWorld,
  )
where

import Coords
  ( IntCoord,
  )
import Data.Maybe
  ( fromMaybe,
  )
import Game
  ( colNb,
    totalRowNb,
  )
import Graphics.Gloss.Interface.IO.Game
  ( Color,
    Picture (..),
    black,
    makeColorI,
    mixColors,
    translate,
    white,
  )
import Grid
  ( Grid,
    lMapGrid,
    mapGrid,
    update,
  )
import Pieces
  ( MovingPiece (..),
    PieceType (..),
    ghostRepr,
    selfRepr,
  )
import World
  ( World (..),
  )

-- | Type alias for a grid composed of colored squares.
type ColorGrid = Grid Color

-- | Size of the margin around the grid, in pixels.
margin :: Int
margin = 40

-- | Square size, in pixels.
squareSize :: Float
squareSize = 35

-- | Square border width, in pixels.
borderSize :: Float
borderSize = 2

-- | Square border color
borderColor :: Color
borderColor = black

-- | Color in which to render full rows, just before they are deleted.
fullRowColor :: Color
fullRowColor = makeColorI 219 193 195 255

-- | Background color of the window.
backgroundColor :: Color
backgroundColor = white

-- | Color map for each tetromino.
colorFromPieceType :: PieceType -> Color
colorFromPieceType p = case p of
  I -> makeColorI 23 205 255 255
  J -> makeColorI 30 49 148 255
  L -> makeColorI 255 128 0 255
  O -> makeColorI 255 217 0 255
  S -> makeColorI 12 173 42 255
  T -> makeColorI 138 25 138 255
  Z -> makeColorI 191 23 37 255

-- | Generates the color for the piece "ghost" representation, based on the piece base color.
ghostColor ::
  -- | Base color of the piece
  Color ->
  -- | Color of its ghost representation
  Color
ghostColor = mixColors 0.4 0.6 backgroundColor

-- | Grid width (without margin), in pixels
gridWidth :: Int
gridWidth = round $ squareSize * fromIntegral colNb

-- | Grid height (without margin), in pixels
gridHeight :: Int
gridHeight = round $ squareSize * fromIntegral totalRowNb

-- | Adds grid margin size to a grid dimension.
withMargin :: Int -> Int
withMargin dim = dim + 2 * margin

-- | Creates the 'Picture' object representing a colored square at the correct window position
-- from a grid coordinate and a color.
gridSquareWithColor ::
  -- | Grid coordinates of this square
  IntCoord ->
  -- | Color in which to render the square
  Color ->
  -- | Resulting picture
  Picture
gridSquareWithColor (xGrid, yGrid) color = Pictures [background, foreground]
  where
    background = Color borderColor $ Polygon [(x, y), (x, y + squareSize), (x + squareSize, y + squareSize), (x + squareSize, y)]
    foreground = Color color $ Polygon [(x + borderSize, y + borderSize), (x + borderSize, y + squareSize - borderSize), (x + squareSize - borderSize, y + squareSize - borderSize), (x + squareSize - borderSize, y + borderSize)]
    x = fromIntegral xGrid * squareSize
    y = fromIntegral yGrid * squareSize

-- | Uses the piece grid, the rows marked for deletion and the moving piece of the specified 'World' object
-- to create the resulting color grid.
getColorGrid :: World -> ColorGrid
getColorGrid world@World {grid, movingPiece = movingPiece@MovingPiece {pieceType}, rowsToDelete} =
  ((solidifiedColorGrid `update` rowsToDeleteUpdate) `update` ghostUpdate) `update` movingUpdate
  where
    solidifiedColorGrid = mapGrid (maybe backgroundColor colorFromPieceType) grid
    rowsToDeleteUpdate = [((x, y), fullRowColor) | x <- [0 .. (colNb - 1)], y <- rowsToDelete]
    ghostUpdate = [((x, y), ghostColor $ colorFromPieceType pieceType) | (x, y) <- ghostRepr grid movingPiece]
    movingUpdate = [((x, y), colorFromPieceType pieceType) | (x, y) <- selfRepr movingPiece]

-- | Get the graphical representation corresponding to the current game state.
renderWorld :: World -> IO Picture
renderWorld world@World {finished, rowsCleared, paused} =
  return $
    if finished
      then Text ("End of game.\nYou cleared " ++ show rowsCleared ++ "rows!")
      else
        putInCenter $
          Pictures $
            if paused
              then worldRepr ++ [Text "Paused"]
              else worldRepr
  where
    worldRepr = lMapGrid gridSquareWithColor $ getColorGrid world
    putInCenter = translate (-fromIntegral colNb * squareSize / 2) (-fromIntegral totalRowNb * squareSize / 2)
