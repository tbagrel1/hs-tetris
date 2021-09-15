module Main where

import Lib
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

main :: IO ()
main = playIO displayMode backgroundColor stepsPerSecond initialWorld render handleEvent advanceWorld

displayMode :: Display
displayMode = InWindow "Tetris" (200, 200) (10, 10)

backgroundColor :: Color
backgroundColor = white

stepsPerSecond :: Int
stepsPerSecond = 1

render :: World -> IO Picture
render _ = circle 80
