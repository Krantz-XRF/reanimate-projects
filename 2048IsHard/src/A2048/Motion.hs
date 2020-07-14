{-|
Module      : A2048.Motion
Description : 2048 Game moves.
Copyright   : (c) Xie Ruifeng, 2020
License     : AGPL-3
Maintainer  : krantz.xrf@outlook.com
Stability   : experimental
Portability : portable
-}
module A2048.Motion where

import Control.Monad
import Control.Monad.State.Class
import Control.Monad.Reader.Class

import Control.Lens

import Reanimate

import A2048.Config
import A2048.Tile
import A2048.Board
import A2048.Logic

-- |All 2048 game moves happen in a 'MonadMotion'.
type MonadMotion m = (Monad2048 m, MonadScene m)

-- |Perform a game move to a given 'Direction'.
performMove :: MonadMotion m => Direction -> m ()
performMove d = waitOnA $ do
  dt <- asks (view motionDuration)
  forkA $ playA . staticFrame dt =<< boardSVG
  w <- asks (view boardWidth)
  h <- asks (view boardHeight)
  es <- events w h d <$> get
  sequence_
    [ forkA (eventAnim (x, y) a)
    | (y, row) <- zip [0 ..] es
    , (x, a) <- zip [0 ..] row ]
  let toTile TileVanish = 0
      toTile (TileMove l _) = l
      toTile (TileMerge l _ _) = l
  put (map (map toTile) es)

-- |Move one tile to another position.
moveTile :: MonadMotion m
         => Bool        -- ^Whether this tile should fade out.
         -> Int         -- ^Level of this tile.
         -> (Int, Int)  -- ^Departure.
         -> (Int, Int)  -- ^Destination.
         -> m ()
moveTile fade l (x, y) (x', y') = do
  dt <- asks (view motionDuration)
  let lerp r a b = r * fromIntegral a + (1 - r) * fromIntegral b
  let p r = (lerp r x' x, lerp r y' y)
  cfg <- ask
  tl <- tile l
  let f t = uncurry (translateGrid cfg) (p t) tl
  let fadeOrNot = if fade then fadeOutE else constE id
  playA $ setDuration dt $ signalA (curveS 2) $ applyE fadeOrNot (animate f)

-- |Emerge a tile from thin air at a given position.
emergeTile :: MonadMotion m => Int -> (Int, Int) -> m ()
emergeTile l (x, y) = do
  dt <- asks (view motionDuration)
  tl <- tile l
  cfg <- ask
  let trans = translateGrid cfg (fromIntegral x) (fromIntegral y)
  let func t = trans (scale t tl)
  let growAnim = setDuration (dt / 4) (signalA (fromToS 0 1.2) (animate func))
  let shrinkAnim = setDuration (dt / 4) (signalA (fromToS 1.2 1) (animate func))
  playA growAnim
  playA shrinkAnim

-- |Emit an animation according to a game event.
eventAnim :: MonadMotion m => (Int, Int) -> GameEvent Int (Int, Int) -> m ()
eventAnim (x, y) TileVanish = do
  f <- asks (view motionFillPadding)
  when f $ emergeTile (1 + (x + y + 1) `rem` 2) (x, y)
eventAnim p (TileMove l p') = moveTile False l p' p
eventAnim p (TileMerge l p1 p2) = do
  forkA $ moveTile True (pred l) p1 p
  forkA $ moveTile True (pred l) p2 p
  dt <- asks (view motionDuration)
  waitA (dt / 2)
  emergeTile l p
