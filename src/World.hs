{-# LANGUAGE NamedFieldPuns #-}

{- |
Module : World
Description : Types and functions to represent the game state.
Copyright : (c) Thomas BAGREL @ TWEAG, 2021
License : AGPL-3.0
Maintainer : Thomas BAGREL <thomas.bagrel@tweag.io>
Stability : experimental
-}

module World
  ( World (..),
    makeInitialWorld,
    updateWorldWithEvent,
    updateWorldWithTick,
  )
where

import qualified Data.Vector as V
import Game
  ( MoveType (..),
    colNb,
    KeyMapping,
    initialSpeed,
    dropTickInterval,
    totalRowNb,
    ActionType (..),
    ControlType (..),
  )
import Grid
  ( deleteRows,
    fullRows,
    (!!!)
  )
import Pieces
  ( MovingPiece,
    PieceGrid,
    PieceType,
    applyMove,
    generatePieceBag,
    selfRepr,
    solidifyPiece,
    spawnPiece,
    isValid,
  )
import Control.Monad
  ( (>=>),
  )
import Data.Maybe
  ( isJust,
    fromMaybe,
  )
import Graphics.Gloss.Interface.IO.Game
  ( Event (..),
    KeyState (..),
  )

{- | Represents the game state
-}
data World = World
  { keyMapping :: KeyMapping, -- ^ Mapping from keyboard keys to game actions
    grid :: PieceGrid, -- ^ Grid of solidified pieces (i.e. pieces which are no longer falling)
    movingPiece :: MovingPiece, -- ^ The tetromino which is currently falling
    pieceBag :: [PieceType], -- ^ Next piece types to spawn
    rowsToDelete :: [Int], -- ^ Completed rows, which will be deleted next tick
    currentSpeed :: Float, -- ^ Current speed of the game. Determines the tick interval between two "gravity" moves
    tickNo :: Integer, -- ^ Tick count
    tickNextDrop :: Integer, -- ^ Tick number where the next "gravity" move will happen
    paused :: Bool, -- ^ Is the game paused?
    finished :: Bool, -- ^ Is the game over?
    rowsCleared :: Int -- ^ Number of complete rows cleared by the player
  }

{- | Creates the initial game state (using the specified key mapping).
-}
makeInitialWorld :: KeyMapping -> IO World
makeInitialWorld keyMapping = do
  (firstPieceType : pieceBag) <- generatePieceBag
  return
    World
      { keyMapping = keyMapping,
        grid = V.replicate totalRowNb $ V.replicate colNb Nothing,
        movingPiece = spawnPiece firstPieceType,
        pieceBag = pieceBag,
        rowsToDelete = [],
        currentSpeed = initialSpeed,
        tickNo = 0,
        tickNextDrop = fromIntegral $ dropTickInterval initialSpeed,
        paused = False,
        finished = False,
        rowsCleared = 0
      }

{- | Updates the game state when an event is issued.
-}
updateWorldWithEvent
  :: Event -- ^ The received event
  -> World -- ^ Current game state
  -> IO World -- ^ Next game state
updateWorldWithEvent (EventKey key Down _ _) world@World {grid, movingPiece, keyMapping, paused} = case keyMapping key of
  Just (Move moveType) -> return world {movingPiece = if not paused then fromMaybe movingPiece $ applyMove grid movingPiece moveType else movingPiece}
  Just (Control PauseResume) -> return world {paused = not paused}
  Just (Control Reset) -> makeInitialWorld keyMapping
  Nothing -> return world
updateWorldWithEvent _ world = return world

{- | Updates the game state when a tick is elapsed
-}
updateWorldWithTick
  :: Float -- ^ Number of seconds elapsed since the last call to this function
  -> World -- ^ Current game state
  -> IO World -- ^ Next game state
updateWorldWithTick _ world@World {grid, currentSpeed, tickNo, tickNextDrop, movingPiece, rowsToDelete, paused} =
  if paused then return world else
    let tickNo' = tickNo + 1
        grid' = if null rowsToDelete then grid else deleteRows grid rowsToDelete Nothing
        rowsToDelete' = []
    in if tickNo' >= tickNextDrop
          then
            let tickNextDrop' = tickNextDrop + fromIntegral (dropTickInterval currentSpeed)
            in case applyMove grid' movingPiece MoveDown of
                  Just movingPiece' ->
                    return
                      world -- if it's possible to move down, then move down
                        { tickNo = tickNo',
                          grid = grid',
                          rowsToDelete = rowsToDelete',
                          tickNextDrop = tickNextDrop',
                          movingPiece = movingPiece'
                        }
                  Nothing ->
                    (solidifyAndNewMovingPiece >=> updateFinished >=> updateRowsToDelete) $
                      world -- otherwise, anchor the piece in the grid
                        { tickNo = tickNo',
                          grid = grid',
                          rowsToDelete = rowsToDelete',
                          tickNextDrop = tickNextDrop'
                        }
          else
            return
              world
                { tickNo = tickNo',
                  grid = grid',
                  rowsToDelete = rowsToDelete'
                }

{- | Solidifies the current moving piece, and replaces it with a freshly spawned one.
Fills the piece bag if necessary.
Must be called only if the current tick is a gravity tick and if the moving piece could no longer move down.
-}
solidifyAndNewMovingPiece :: World -> IO World
solidifyAndNewMovingPiece world@World {grid, pieceBag, movingPiece} = do
  let grid' = solidifyPiece grid movingPiece
  (newPieceType : pieceBag') <- if null pieceBag then generatePieceBag else return pieceBag
  let movingPiece' = spawnPiece newPieceType
  return
    world
      { grid = grid',
        movingPiece = movingPiece',
        pieceBag = pieceBag'
      }

{- | When a new moving piece is spawned, checks if the game is finished
(i.e. if the fresh moving piece already overlap solidified pieces in the grid).
-}
updateFinished :: World -> IO World
updateFinished world@World {grid, movingPiece, finished} = return world {finished = finished || not (isValid grid movingPiece)}

{- | When a moving piece has been solidified, checks if there are complete rows,
and if so, marks them for deletion.
-}
updateRowsToDelete :: World -> IO World
updateRowsToDelete world@World {grid, rowsCleared} =
  let rows = fullRows grid
      n = length rows
   in return
        world
          { rowsToDelete = rows,
            rowsCleared = rowsCleared + n
          }
