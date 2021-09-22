{-# LANGUAGE NamedFieldPuns #-}

module World
    ( World (..)
    , makeInitialWorld
    , updateWorldWithEvent
    , updateWorldWithTick
    ) where

import Pieces
import Grid
import Game

import qualified Data.Vector as V
import Graphics.Gloss.Interface.IO.Game
import Control.Monad ((>=>))
import Data.Maybe (isJust)

data World = World
    { grid :: PieceGrid
    , movingPiece :: MovingPiece
    , pieceBag :: [PieceType]
    , linesToDelete :: [Int]
    , dropTickInterval :: Integer
    , tickNo :: Integer
    , tickNextDrop :: Integer
    , finished :: Bool
    , linesCleared :: Int
    } deriving (Eq, Show)

makeInitialWorld :: IO World
makeInitialWorld = do
    (firstPieceType : pieceBag) <- generatePieceBag
    return World
        { grid = V.replicate (rowNb + 2) $ V.replicate colNb Nothing
        , movingPiece = spawnPiece firstPieceType
        , pieceBag = pieceBag
        , linesToDelete = []
        , dropTickInterval = toInteger initialDropTickInterval
        , tickNo = 0
        , tickNextDrop = toInteger initialDropTickInterval
        , finished = False
        , linesCleared = 0
        }

updateWorldWithEvent :: Event -> World -> IO World
updateWorldWithEvent (EventKey key Down _ _) world@World { grid, movingPiece } = return $ case defaultKeyMapping key >>= applyMove grid movingPiece of
    Just newMovingPiece -> world { movingPiece = newMovingPiece }
    Nothing -> world
updateWorldWithEvent _ world = return world

updateWorldWithTick :: Float -> World -> IO World
updateWorldWithTick _ world@World { grid, dropTickInterval, tickNo, tickNextDrop, movingPiece, linesToDelete } =
    let
        tickNo' = tickNo + 1
        grid' = if null linesToDelete then grid else deleteLines grid linesToDelete Nothing
        linesToDelete' = [] in
    if tickNo' >= tickNextDrop then
        let tickNextDrop' = tickNextDrop + dropTickInterval in
        case applyMove grid' movingPiece MoveDown of
            Just movingPiece' -> return world  -- if it's possible to move down, then move down
                { tickNo = tickNo'
                , grid = grid'
                , linesToDelete = linesToDelete'
                , tickNextDrop = tickNextDrop'
                 , movingPiece = movingPiece'
                }
            Nothing -> (solidifyAndNewMovingPiece >=> updateFinished >=> updateLinesToDelete) $ world  -- otherwise, anchor the piece in the grid
                { tickNo = tickNo'
                , grid = grid'
                , linesToDelete = linesToDelete'
                , tickNextDrop = tickNextDrop'
                }
    else
        return world
            { tickNo = tickNo'
            , grid = grid'
            , linesToDelete = linesToDelete'
            }

--   + 3.1 solidify the piece in the grid
--         and get a new piece from the bag, and regenerate the bag if necessary
solidifyAndNewMovingPiece :: World -> IO World
solidifyAndNewMovingPiece world@World { grid, pieceBag, movingPiece } = do
    let grid' = solidifyPiece grid movingPiece
    (newPieceType : pieceBag') <- if null pieceBag then generatePieceBag else return pieceBag
    let movingPiece' = spawnPiece newPieceType
    return world
        { grid = grid'
        , movingPiece = movingPiece'
        , pieceBag = pieceBag'
        }

--   + 3.2 check end of game:
updateFinished :: World -> IO World
updateFinished world@World { grid, movingPiece, finished } = do
    putStrLn $ "world: " ++ show world
    return world { finished = finished || isFinished grid movingPiece } where
        isFinished grid newMovingPiece = any (\gridPoint -> isJust $ grid !!! gridPoint) $ selfRepr newMovingPiece

--   + 3.3 check if lines are complete
--         if lines are complete:
--             + mark lines for deletion
--             + increase score
updateLinesToDelete :: World -> IO World
updateLinesToDelete world@World { grid, linesCleared } =
    let lines = fullLines grid
        n = length lines in
    return world
        { linesToDelete = lines
        , linesCleared = linesCleared + n
        }
