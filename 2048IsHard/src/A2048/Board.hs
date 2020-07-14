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

import Data.Monoid

import Control.Applicative
import Control.Monad.Reader
import Control.Monad.State
import Control.Lens

import Graphics.SvgTree
import Reanimate

import A2048.Tile
import A2048.Config

type Board = [[Int]]

-- |The 2048 game monad, a concrete instance for 'Monad2048' constraint.
type Game s x = ReaderT Game2048Config (StateT Board (Scene s)) x

-- |All game actions run in a 'Monad2048' monad.
type Monad2048 m = (MonadState Board m, MonadReader Game2048Config m)

-- |'MonadScene' allows emitting animations, sequential or parallel.
class Monad m => MonadScene m where
  -- |Universal quantified "thread ID".
  type WorldType m :: *
  -- |Embed a 'Scene' monad.
  liftScene :: Scene (WorldType m) a -> m a
  -- |Wait for a 'Scene' to finish.
  simultaneously :: m a -> m a

-- |Emit an animation sequentially.
tellS :: MonadScene m => Animation -> m ()
tellS = liftScene . play

-- |Emit an animation sequentially.
tellP :: MonadScene m => Animation -> m ()
tellP = liftScene . fork . play

instance MonadScene (Scene s) where
  type WorldType (Scene s) = s
  liftScene = id
  simultaneously = waitOn

instance MonadScene m => MonadScene (StateT s m) where
  type WorldType (StateT s m) = WorldType m
  liftScene = lift . liftScene
  simultaneously m = StateT (simultaneously . runStateT m)

instance MonadScene m => MonadScene (ReaderT r m) where
  type WorldType (ReaderT r m) = WorldType m
  liftScene = lift . liftScene
  simultaneously m = ReaderT (simultaneously . runReaderT m)

-- |Translate to the selected grid, pure function.
translateGrid :: Game2048Config -> Double -> Double -> SVG -> SVG
translateGrid Game2048Config{..} m n t = translate x y t where
  sx = (fromIntegral _boardWidth - 1) / 2
  sy = (fromIntegral _boardHeight - 1) / 2
  x = (m - sx) * (_tileSize + _boardGapSize)
  y = (sy - n) * (_tileSize + _boardGapSize)

-- |Translate to the selected grid.
translateGridM :: MonadReader Game2048Config m => Double -> Double -> m SVG -> m SVG
translateGridM m n t = ask >>= \cfg -> translateGrid cfg m n <$> t

-- |Traverse all grids.
foreachGrid :: Monad2048 m => (Int -> m SVG) -> m [SVG]
foreachGrid g = get >>= \board -> sequenceA
  [ translateGridM (fromIntegral m) (fromIntegral n) (g x)
  | (n :: Int, row) <- zip [0 ..] board
  , (m :: Int, x) <- zip [0 ..] row ]

-- |Generate an SVG image for the empty board.
boardSVG :: Monad2048 m => m SVG
boardSVG = do
  Game2048Config{..} <- ask
  let w = fromIntegral _boardWidth
  let h = fromIntegral _boardHeight
  let bw = w * _tileSize + (w - 1) * _boardGapSize + _boardBorderSize * 2
  let bh = h * _tileSize + (h - 1) * _boardGapSize + _boardBorderSize * 2
  let bg = Last (Just _boardFillColour)
  let boardRect = roundedRect bw bh _tileRadius & fillColor .~ bg
  mkGroup . (boardRect :) <$> foreachGrid (const emptyTile)

-- |Take a snapshot of the current game status.
snapshot :: Monad2048 m => m SVG
snapshot = mkGroup <$> liftA2 (:) boardSVG (foreachGrid tile)

-- |Emit a static animation for the current game status.
hold :: (Monad2048 m, MonadScene m) => Double -> m ()
hold t = tellS . staticFrame t =<< snapshot

-- |Convert a 'Monad2048' action to an animation.
gameAnimation :: Game2048Config -> (forall s . Game s ()) -> Animation
gameAnimation cfg g = 
  let b = replicate (view boardHeight cfg)
        $ replicate (view boardWidth cfg) 0
  in sceneAnimation $ evalStateT (runReaderT g cfg) b
