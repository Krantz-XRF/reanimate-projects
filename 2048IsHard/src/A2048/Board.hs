{-|
Module      : A2048.Board
Description : Game board for the 2048 game.
Copyright   : (c) Xie Ruifeng, 2020
License     : AGPL-3
Maintainer  : krantz.xrf@outlook.com
Stability   : experimental
Portability : portable
-}
module A2048.Board where

import Data.Monoid
import Data.Coerce

import Control.Applicative
import Control.Monad.Writer.CPS
import Control.Monad.State
import Control.Monad.Reader
import Control.Lens

import Graphics.SvgTree
import Reanimate
import Reanimate.Raster

import A2048.Tile
import A2048.Config
import A2048.Cache

type Board = [[Int]]

-- |Sequenced animations, 'Monoid' instance using 'seqA'.
newtype SeqAnim = SeqAnim { unwrapSeqAnim :: Animation }

instance Semigroup SeqAnim where
  SeqAnim a1 <> SeqAnim a2 = SeqAnim (a1 `seqA` a2)

instance Monoid SeqAnim where
  mempty = SeqAnim (staticFrame 0 None)

-- |Parallel animations, 'Monoid' instance using 'parA'.
newtype ParAnim = ParAnim { unwrapParAnim :: Animation }

instance Semigroup ParAnim where
  ParAnim a1 <> ParAnim a2 = ParAnim (a1 `parA` a2)

instance Monoid ParAnim where
  mempty = ParAnim (staticFrame 0 None)

-- |The 2048 game monad, a concrete instance for 'Monad2048' constraint.
type Game s a x = ReaderT Game2048Config (WriterT a (State Board)) x

-- |'SeqAnim' and 'ParAnim'.
type IsAnim a = Coercible Animation a

-- |All game actions run in a 'Monad2048' monad.
type Monad2048 a m =
  ( IsAnim a
  , MonadWriter a m
  , MonadState Board m
  , MonadReader Game2048Config m)

-- |Emit an animation, sequential by default.
tellA :: (IsAnim a, MonadWriter a m) => Animation -> m ()
tellA = tell . coerce

-- |Temporarily switch to parallel animation.
simultaneously :: Monad m => WriterT ParAnim m a -> WriterT SeqAnim m a
simultaneously m = writerT $ fmap parToSeq <$> runWriterT m
  where parToSeq (ParAnim a) = SeqAnim a

-- |Translate to the selected grid.
translateGrid :: Monad2048 a m => Int -> Int -> m SVG -> m SVG
translateGrid m n t = do
  Game2048Config{..} <- ask
  let sx = (fromIntegral _boardWidth - 1) / 2
  let sy = (fromIntegral _boardHeight - 1) / 2
  let x = (fromIntegral m - sx) * (_tileSize + _boardGapSize)
  let y = (sy - fromIntegral n) * (_tileSize + _boardGapSize)
  translate x y <$> t

-- |Traverse all grids.
foreachGrid :: Monad2048 a m => (Int -> m SVG) -> m [SVG]
foreachGrid g = get >>= \board -> sequenceA
  [ translateGrid m n (g x)
  | (n, row) <- zip [0 ..] board
  , (m, x) <- zip [0 ..] row ]

-- |Generate an SVG image for the empty board.
boardSVG :: Monad2048 a m => m SVG
boardSVG = do
  Game2048Config{..} <- ask
  let w = fromIntegral _boardWidth
  let h = fromIntegral _boardHeight
  let bw = w * _tileSize + (w - 1) * _boardGapSize + _boardBorderSize * 2
  let bh = h * _tileSize + (h - 1) * _boardGapSize + _boardBorderSize * 2
  let bg = Last (Just _boardFillColour)
  let boardRect = roundedRect bw bh _tileRadius & fillColor .~ bg
  cacheId <- asks bgCacheId
  prerenderSvg cacheId . mkGroup . (boardRect :)
    <$> foreachGrid (const emptyTile)

-- |Take a snapshot of the current game status.
snapshot :: Monad2048 a m => m SVG
snapshot = mkGroup <$> liftA2 (:) boardSVG (foreachGrid tile)

-- |Emit a static animation for the current game status.
hold :: Monad2048 a m => Double -> m ()
hold t = tellA . staticFrame t =<< snapshot

-- |Convert a 'Monad2048' action to an animation.
gameAnimation :: Game2048Config -> (forall s . Game s SeqAnim a) -> Animation
gameAnimation cfg g = coerce $ snd $ runIdentity $ do
  let b = replicate (view boardHeight cfg) $ replicate (view boardWidth cfg) 0
  evalStateT (runWriterT $ runReaderT g cfg) b
