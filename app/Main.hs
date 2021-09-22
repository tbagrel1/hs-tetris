{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import Render (withMargin, gridWidth, gridHeight, backgroundColor, renderWorld)
import World (makeInitialWorld, updateWorldWithEvent, updateWorldWithTick)
import Game (ticksPerSecond)

main :: IO ()
main = do
    initialWorld <- makeInitialWorld
    let displayMode = InWindow "Tetris" (withMargin gridWidth, withMargin gridHeight) (0, 0)
    playIO displayMode backgroundColor ticksPerSecond initialWorld renderWorld updateWorldWithEvent updateWorldWithTick
