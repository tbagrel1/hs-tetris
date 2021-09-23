module Main where

import Game
  ( ticksPerSecond,
    defaultKeyMapping,
  )
import Graphics.Gloss.Interface.IO.Game
  ( Display (..),
    playIO,
  )
import Render
  ( backgroundColor,
    gridHeight,
    gridWidth,
    renderWorld,
    withMargin,
  )
import World
  ( makeInitialWorld,
    updateWorldWithEvent,
    updateWorldWithTick
  )

{- | Creates a new initial world then run the game
-}
main :: IO ()
main = do
  initialWorld <- makeInitialWorld defaultKeyMapping
  let displayMode = InWindow "Tetris" (withMargin gridWidth, withMargin gridHeight) (0, 0)
  playIO displayMode backgroundColor ticksPerSecond initialWorld renderWorld updateWorldWithEvent updateWorldWithTick
