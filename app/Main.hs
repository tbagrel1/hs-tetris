module Main where

import Lib
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

main :: IO ()
main = putStrLn "hello" --playIO displayMode backgroundColor stepsPerSecond initialWorld render handleEvent advanceWorld

displayMode :: Display
displayMode = InWindow "Tetris" (200, 200) (10, 10)

backgroundColor :: Color
backgroundColor = white

stepsPerSecond :: Int
stepsPerSecond = 1

render :: World -> IO Picture
render _ = return $ circle 80

-- Grid
-- Piece
-- Lignes marquées pour suppression
-- pièce fantome