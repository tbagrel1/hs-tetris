{- |
Module : Game
Description : Constants and types defining the Tetris gameplay.
Copyright : (c) Thomas BAGREL @ TWEAG, 2021
License : AGPL-3.0
Maintainer : Thomas BAGREL <thomas.bagrel@tweag.io>
Stability : experimental
-}

module Game
  ( KeyMapping,
    ActionType (..),
    MoveType (..),
    ControlType (..),
    ticksPerSecond,
    initialSpeed,
    dropTickInterval,
    playableRowNb,
    totalRowNb,
    colNb,
    isInPlayableGrid,
    isInTotalGrid,
    isInGridOrAbove,
    defaultKeyMapping,
  )
where

import Coords
  ( IntCoord
  )
import Graphics.Gloss.Interface.IO.Game
  ( Key(..)
  )

{- | Type alias representing the key mapping used by the game
-}
type KeyMapping = Key -> Maybe ActionType

{- | Either an in-game move action, or a control action (play, pause, reset...)
-}
data ActionType
  = Move MoveType
  | Control ControlType

{- | All possible moves in the game
-}
data MoveType
  = RotateLeft -- ^ Counter-clockwise rotation
  | RotateRight -- ^ Clockwise rotation
  | MoveDown -- ^ Move down (1 step)
  | MoveLeft -- ^ Move left (1 step)
  | MoveRight -- ^ Move right (1 step)
  | FullDrop -- ^ Move down (as much as possible)
  deriving (Eq, Show)

{- | All control actions (not related to in-game movement)
-}
data ControlType
  = PauseResume -- ^ Pause or resume the current game
  | Reset -- ^ Reset the game

{- | Controls the update rate of the game screen and data
-}
ticksPerSecond :: Int
ticksPerSecond = 30

{- | Controls the speed at which pieces fall at the beginning of the game -}
initialSpeed :: Float
initialSpeed = 0.7

{- | Computes the interval between two drop ticks using ticksPerSecond and the current speed
-}
dropTickInterval
  :: Float -- ^ Current speed factor
  -> Int -- ^ Resulting drop tick interval
dropTickInterval speed = round (fromIntegral ticksPerSecond / speed)


{- | Number of rows in the grid that the player can actually use
-}
playableRowNb :: Int
playableRowNb = 20

{- | Number of rows in the grid including the two where pieces spawn
-}
totalRowNb :: Int
totalRowNb = playableRowNb + 2

{- | Number of columns in the grid
-}
colNb :: Int
colNb = 10

{- | Tells whether a 2D-point is in the playable part of the grid or not
-}
isInPlayableGrid :: IntCoord -> Bool
isInPlayableGrid (x, y) = x >= 0 && x < colNb && y >= 0 && y < (playableRowNb + 2)

{- | Tells whether a 2D-point is in the grid, including the spawn zone, or not.
-}
isInTotalGrid :: IntCoord -> Bool
isInTotalGrid (x, y) = x >= 0 && x < colNb && y >= 0 && y < (totalRowNb + 2)

{- | Tells whether a 2D-point is in the grid and its vertical extension, or not.
-}
isInGridOrAbove :: IntCoord -> Bool
isInGridOrAbove (x, y) = x >= 0 && x < colNb && y >= 0

{- | Default key mapping, based on FPS controls
-}
defaultKeyMapping :: KeyMapping
defaultKeyMapping key = case key of
  Char 'a' -> Just $ Move RotateLeft
  Char 'e' -> Just $ Move RotateRight
  Char 's' -> Just $ Move MoveDown
  Char 'q' -> Just $ Move MoveLeft
  Char 'd' -> Just $ Move MoveRight
  Char 'z' -> Just $ Move FullDrop
  Char 'p' -> Just $ Control PauseResume
  Char 'r' -> Just $ Control Reset
  _ -> Nothing
