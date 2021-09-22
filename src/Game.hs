module Game
    ( KeyMapping
    , MoveType (..)
    , ticksPerSecond
    , initialDropTickInterval
    , rowNb
    , colNb
    , defaultKeyMapping
    ) where

import Graphics.Gloss.Interface.IO.Game

type KeyMapping = Key -> Maybe MoveType

data MoveType
    = RotateLeft
    | RotateRight
    | MoveDown
    | MoveLeft
    | MoveRight
    | FullDrop
    deriving (Eq, Show)

ticksPerSecond :: Int
ticksPerSecond = 30

initialDropTickInterval :: Int
initialDropTickInterval = 50

rowNb :: Int
rowNb = 20

colNb :: Int
colNb = 10

defaultKeyMapping :: KeyMapping
defaultKeyMapping key = case key of
    Char 'a' -> Just RotateLeft
    Char 'e' -> Just RotateRight
    Char 's' -> Just MoveDown
    Char 'q' -> Just MoveLeft
    Char 'd' -> Just MoveRight
    Char 'z' -> Just FullDrop
    _ -> Nothing
