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

type MonadMotion m = (Monad2048 m, MonadScene m)

-- |Perform a game move to a given 'Direction'.
performMove :: MonadMotion m => Direction -> m ()
performMove d = simultaneously $ do
  dt <- asks (view motionDuration)
  tellP . staticFrame dt =<< boardSVG
  w <- asks (view boardWidth)
  h <- asks (view boardHeight)
  es <- events w h d <$> get
  sequence_
    [ eventAnim (x, y) a
    | (y, row) <- zip [0 ..] es
    , (x, a) <- zip [0 ..] row ]
  let toTile TileVanish = 0
      toTile (TileMove l _) = l
      toTile (TileMerge l _ _) = l
  put (map (map toTile) es)

rush :: Animation -> Animation
rush = signalA (fromToS 0.5 1 . curveS 2)

moveTile :: MonadMotion m => Int -> (Int, Int) -> (Int, Int) -> m ()
moveTile l (x, y) (x', y') = do
  dt <- asks (view motionDuration)
  let lerp r a b = r * fromIntegral a + (1 - r) * fromIntegral b
  let p r = (lerp r x' x, lerp r y' y)
  cfg <- ask
  tl <- tile l
  let f t = uncurry (translateGrid cfg) (p t) tl
  tellP $ setDuration dt $ rush $ animate f

emergeTile :: MonadMotion m => Int -> (Int, Int) -> m ()
emergeTile l (x, y) = do
  dt <- asks (view motionDuration)
  tl <- translateGridM (fromIntegral x) (fromIntegral y) (tile l)
  tellP $ setDuration dt $ rush $ animate (`scale` tl)

-- |Emit an animation according to a game event.
eventAnim :: MonadMotion m => (Int, Int) -> GameEvent Int (Int, Int) -> m ()
eventAnim (x, y) TileVanish = do
  f <- asks (view motionFillPadding)
  when f $ emergeTile (1 + (x + y + 1) `rem` 2) (x, y)
eventAnim p (TileMove l p') = moveTile l p' p
eventAnim p (TileMerge l p1 p2) = do
  moveTile (pred l) p1 p
  moveTile (pred l) p2 p
  emergeTile l p
