{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Lib
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

main :: IO ()
main = do
    initialWorld <- makeInitialWorld
    playIO displayMode backgroundColor ticksPerSecond initialWorld render handleEvent advanceWorld

displayMode :: Display
displayMode = InWindow "Tetris" (800, 800) (10, 10)

squareSize :: Float
squareSize = 35

borderSize :: Float
borderSize = 2

borderColor :: Color
borderColor = black

square :: GridPoint -> Color -> Picture
square (xGrid, yGrid) color = Pictures [background, foreground] where
    background = Color borderColor $ Polygon [(x, y), (x, y + squareSize), (x + squareSize, y + squareSize), (x + squareSize, y)]
    foreground = Color color $ Polygon [(x + borderSize, y + borderSize), (x + borderSize, y + squareSize - borderSize), (x + squareSize - borderSize, y + squareSize - borderSize), (x + squareSize - borderSize, y + borderSize)]
    x = fromIntegral xGrid * squareSize
    y = fromIntegral yGrid * squareSize

render :: World -> IO Picture
render world@World { finished, linesCleared } = return $ if finished then Text ("End of game.\nYou cleared " ++ show linesCleared ++ "lines!") else putInCenter $ Pictures $ lmapGrid square $ getColorGrid world where
    putInCenter = translate (-fromIntegral colNb * squareSize / 2) (-fromIntegral rowNb * squareSize / 2)
