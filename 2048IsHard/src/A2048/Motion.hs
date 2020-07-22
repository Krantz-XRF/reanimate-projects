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

import Control.Monad.State.Class
import Control.Monad.Reader.Class

import Control.Lens

import Reanimate

import A2048.Config
import A2048.Tile
import A2048.Board
import A2048.Logic

-- |Perform a game move to a given 'Direction'.
performMove :: Monad2048 m => Direction -> m Animation
performMove d = do
  dt <- asks (view motionDuration)
  bkg <- staticFrame dt <$> boardSVG
  w <- asks (view boardWidth)
  h <- asks (view boardHeight)
  es <- events w h d <$> get
  anim <- sequence
    [ eventAnim (x, y) a
    | (y, row) <- zip [0 ..] es
    , (x, a) <- zip [0 ..] row ]
  let toTile TileVanish = 0
      toTile (TileMove l _) = l
      toTile (TileMerge l _ _) = l
  put (map (map toTile) es)
  pure (foldl parA bkg anim)

-- |Move one tile to another position.
moveTile :: Monad2048 m
         => Bool        -- ^Whether this tile should fade out.
         -> Int         -- ^Level of this tile.
         -> (Int, Int)  -- ^Departure.
         -> (Int, Int)  -- ^Destination.
         -> m Animation
moveTile fade l (x, y) (x', y') = do
  dt <- asks (view motionDuration)
  let lerp r a b = r * fromIntegral a + (1 - r) * fromIntegral b
  let p r = (lerp r x' x, lerp r y' y)
  cfg <- ask
  tl <- tile l
  let f t = uncurry (translateGrid cfg) (p t) tl
  let fadeOrNot = if fade then fadeOutE else constE id
  pure $ setDuration dt $ signalA (curveS 2) $ applyE fadeOrNot (animate f)

-- |Emerge a tile from thin air at a given position.
emergeTile :: Monad2048 m => Int -> (Int, Int) -> m Animation
emergeTile l (x, y) = do
  dt <- asks (view motionDuration)
  tl <- tile l
  cfg <- ask
  let trans = translateGrid cfg (fromIntegral x) (fromIntegral y)
  let func t = trans (scale t tl)
  let growAnim = setDuration (dt / 4) (signalA (fromToS 0 1.2) (animate func))
  let shrinkAnim = setDuration (dt / 4) (signalA (fromToS 1.2 1) (animate func))
  pure (growAnim `seqA` shrinkAnim)

-- |Emit an animation according to a game event.
eventAnim :: Monad2048 m => (Int, Int) -> GameEvent Int (Int, Int) -> m Animation
eventAnim (x, y) TileVanish = do
  f <- asks (view motionFillPadding)
  if f then emergeTile (1 + (x + y + 1) `rem` 2) (x, y) else pure (pause 0)
eventAnim p (TileMove l p') = moveTile False l p' p
eventAnim p (TileMerge l p1 p2) = do
  m1 <- moveTile True (pred l) p1 p
  m2 <- moveTile True (pred l) p2 p
  dt <- asks (view motionDuration)
  e <- emergeTile l p
  pure (m1 `parA` m2 `parA` (pause (dt / 2) `seqA` e))
