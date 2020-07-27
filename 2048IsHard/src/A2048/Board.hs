{-|
Module      : A2048.Board
Description : Game board for the 2048 game.
Copyright   : (c) Xie Ruifeng, 2020
License     : AGPL-3
Maintainer  : krantz.xrf@outlook.com
Stability   : experimental
Portability : portable
-}
{-# LANGUAGE TypeFamilies #-}
module A2048.Board where

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State
import Control.Lens

import Graphics.SvgTree
import Reanimate

import A2048.Tile
import A2048.Config

-- |Game board: board = [row], row = [tile].
type Board = [[Int]]

-- |The 2048 game monad, a concrete instance for 'Monad2048' constraint.
type Game x = ReaderT Game2048Config (State Board) x

-- |All game actions run in a 'Monad2048' monad.
type Monad2048 m = (MonadState Board m, MonadReader Game2048Config m)

-- |Position of the selected grid, pure function.
gridPos :: Game2048Config -> (Double, Double) -> (Double, Double)
gridPos Game2048Config{..} (m, n) = (x, y) where
  sx = (fromIntegral _boardWidth - 1) / 2
  sy = (fromIntegral _boardHeight - 1) / 2
  x = (m - sx) * (_tileSize + _boardGapSize)
  y = (sy - n) * (_tileSize + _boardGapSize)

-- |Translate to the selected grid, pure function.
translateGrid :: Game2048Config -> Double -> Double -> SVG -> SVG
translateGrid cfg m n = translate x y where (x, y) = gridPos cfg (m, n)

-- |Translate to the selected grid.
translateGridM :: MonadReader Game2048Config m => Double -> Double -> m SVG -> m SVG
translateGridM m n t = ask >>= \cfg -> translateGrid cfg m n <$> t

-- |Traverse all grids.
foreachGrid :: Monad2048 m => (Int -> m SVG) -> m [SVG]
foreachGrid g = get >>= \board -> sequenceA
  [ translateGridM (fromIntegral m) (fromIntegral n) (g x)
  | (n :: Int, row) <- zip [0 ..] board
  , (m :: Int, x) <- zip [0 ..] row ]

-- |Traverse all non-empty grids.
foreachNonEmptyGrid :: Monad2048 m => (Int -> m SVG) -> m [SVG]
foreachNonEmptyGrid f = foreachGrid $ \n -> if n /= 0 then f n else pure None

-- |Generate an SVG image for the empty board.
boardSVG :: Monad2048 m => m SVG
boardSVG = do
  Game2048Config{..} <- ask
  let w = fromIntegral _boardWidth
  let h = fromIntegral _boardHeight
  let bw = w * _tileSize + (w - 1) * _boardGapSize + _boardBorderSize * 2
  let bh = h * _tileSize + (h - 1) * _boardGapSize + _boardBorderSize * 2
  let bg = pure _boardFillColour
  let boardRect = roundedRect bw bh _tileRadius & fillColor .~ bg
  mkGroup . (boardRect :) <$> foreachGrid (const emptyTile)

-- |Take a snapshot of the current game status.
snapshot :: Monad2048 m => m SVG
snapshot = mkGroup <$> liftA2 (:) boardSVG (foreachGrid tile)

-- |Emit a static animation for the current game status.
hold :: Monad2048 m => Double -> m Animation
hold t = staticFrame t <$> snapshot

-- |Convert a 'Monad2048' action to an animation.
gameAnimation :: Game2048Config -> Game a -> a
gameAnimation cfg g = 
  let b = replicate (view boardHeight cfg)
        $ replicate (view boardWidth cfg) 0
  in evalState (runReaderT g cfg) b

-- |Make a pure 'Animation' from a 'Game' monad.
mkPure :: Monad2048 m => (Time -> Game a) -> m (Time -> a)
mkPure f = do
  cfg <- ask; bd <- get
  pure (\t -> gameAnimation cfg (put bd >> f t))
