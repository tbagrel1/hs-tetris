{-# LANGUAGE NamedFieldPuns #-}

module Render
    ( backgroundColor
    , gridWidth
    , gridHeight
    , withMargin
    , renderWorld
    ) where

import Coords
import Grid
import Pieces
import Game
import World
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Interface.IO.Game
import Data.Maybe (fromMaybe)

type ColorGrid = Grid Color

margin :: Int
margin = 40

squareSize :: Float
squareSize = 35

borderSize :: Float
borderSize = 2

borderColor :: Color
borderColor = black

fullLineColor :: Color
fullLineColor = makeColorI 219 193 195 255

backgroundColor :: Color
backgroundColor = white

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
ghostColor = mixColors 0.4 0.6 white

gridWidth :: Int
gridWidth = round $ squareSize * fromIntegral colNb

gridHeight :: Int
gridHeight = round $ squareSize * (fromIntegral rowNb + 2)

withMargin :: Int -> Int
withMargin dim = dim + 2 * margin

gridSquareWithColor:: IntCoord -> Color -> Picture
gridSquareWithColor(xGrid, yGrid) color = Pictures [background, foreground] where
    background = Color borderColor $ Polygon [(x, y), (x, y + squareSize), (x + squareSize, y + squareSize), (x + squareSize, y)]
    foreground = Color color $ Polygon [(x + borderSize, y + borderSize), (x + borderSize, y + squareSize - borderSize), (x + squareSize - borderSize, y + squareSize - borderSize), (x + squareSize - borderSize, y + borderSize)]
    x = fromIntegral xGrid * squareSize
    y = fromIntegral yGrid * squareSize

getColorGrid :: World -> ColorGrid
getColorGrid world@World { grid, movingPiece = movingPiece@MovingPiece { pieceType }, linesToDelete } =
    ((solidifiedColorGrid `update` linesToDeleteUpdate) `update` ghostUpdate) `update` movingUpdate where
        solidifiedColorGrid = mapGrid (fromMaybe backgroundColor . fmap colorFromPieceType) grid
        linesToDeleteUpdate = [((x, y), fullLineColor) | x <- [0..(colNb - 1)], y <- linesToDelete]
        ghostUpdate = [((x, y), ghostColor $ colorFromPieceType pieceType) | (x, y) <- ghostRepr grid movingPiece]
        movingUpdate = [((x, y), colorFromPieceType pieceType) | (x, y) <- selfRepr movingPiece]

renderWorld :: World -> IO Picture
renderWorld world@World { finished, linesCleared } = return $ if finished then Text ("End of game.\nYou cleared " ++ show linesCleared ++ "lines!") else putInCenter $ Pictures $ lMapGrid gridSquareWithColor$ getColorGrid world where
    putInCenter = translate (-fromIntegral colNb * squareSize / 2) (-fromIntegral rowNb * squareSize / 2)
