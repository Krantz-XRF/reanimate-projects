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
import qualified Data.Vector.Mutable as V

import Control.Applicative
import Control.Monad.Writer.CPS
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.ST.Class
import Control.Lens

import Graphics.SvgTree
import Reanimate
import Reanimate.Raster

import A2048.Tile
import A2048.Config
import A2048.Cache

-- |Mutable game board information, for use in the 'ST' monad.
newtype STBoard s = STBoard { unwrapSTBoard :: V.STVector s Int }

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
type Game s a x = ReaderT BoardConfig (WriterT a (StateT (STBoard s) (ST s))) x

-- |'SeqAnim' and 'ParAnim'.
type IsAnim a = Coercible Animation a

-- |All game actions run in a 'Monad2048' monad.
type Monad2048 a m =
  ( MonadST m, IsAnim a
  , MonadWriter a m
  , MonadState (STBoard (WorldType m)) m
  , MonadReader BoardConfig m)

-- |Set up a game board, board = [row], row = [tile].
putBoard :: Monad2048 a m => [[Int]] -> m ()
putBoard b = sequence_
  [ setGrid m n x
  | (n, row) <- zip [0..] b
  , (m, x) <- zip [0..] row ]

-- |Get a tile in the board. Zero for empty, and non-zero for some tile.
getGrid :: Monad2048 a m => Int -> Int -> m Int
getGrid m n = do
  b <- gets unwrapSTBoard
  w <- asks (view boardWidth)
  liftST $ V.read b (m + n * w)

-- |Set a tile in the board. Zero for empty, and non-zero for some tile.
setGrid :: Monad2048 a m => Int -> Int -> Int -> m ()
setGrid m n v = do
  b <- gets unwrapSTBoard
  w <- asks (view boardWidth)
  liftST $ V.write b (m + n * w) v

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
  BoardConfig{_boardTileConfig = TileConfig{..}, ..} <- ask
  let sx = (fromIntegral _boardWidth - 1) / 2
  let sy = (fromIntegral _boardHeight - 1) / 2
  let x = (fromIntegral m - sx) * (_tileSize + _boardGapSize)
  let y = (sy - fromIntegral n) * (_tileSize + _boardGapSize)
  translate x y <$> t

-- |Traverse all grids.
foreachGrid :: Monad2048 a m => (Int -> m SVG) -> m [SVG]
foreachGrid g = do
  w <- asks (view boardWidth)
  h <- asks (view boardHeight)
  let t m n = getGrid m n >>= g
  sequenceA [translateGrid m n (t m n) | m <- [0 .. w-1], n <- [0 .. h-1]]

-- |Generate an SVG image for the empty board.
boardSVG :: Monad2048 a m => m SVG
boardSVG = do
  BoardConfig{_boardTileConfig = TileConfig{..}, ..} <- ask
  let w = fromIntegral _boardWidth
  let h = fromIntegral _boardHeight
  let bw = w * _tileSize + (w - 1) * _boardGapSize + _boardBorderSize * 2
  let bh = h * _tileSize + (h - 1) * _boardGapSize + _boardBorderSize * 2
  let bg = Last (Just _boardFillColour)
  let boardRect = roundedRect bw bh _tileRadius & fillColor .~ bg
  prerenderSvg (BoardBGCache _boardWidth _boardHeight)
    . mkGroup . (boardRect :) <$> foreachGrid (const emptyTile)

-- |Take a snapshot of the current game status.
snapshot :: Monad2048 a m => m SVG
snapshot = mkGroup <$> liftA2 (:) boardSVG (foreachGrid tile)

-- |Emit a static animation for the current game status.
hold :: Monad2048 a m => Double -> m ()
hold t = tellA . staticFrame t =<< snapshot

-- |Convert a 'Monad2048' action to an animation.
gameAnimation :: BoardConfig -> (forall s . Game s SeqAnim a) -> Animation
gameAnimation cfg g = coerce $ snd $ runST $ do
  b <- V.new (view boardWidth cfg * view boardHeight cfg)
  V.set b 0
  evalStateT (runWriterT $ runReaderT g cfg) (STBoard b)
