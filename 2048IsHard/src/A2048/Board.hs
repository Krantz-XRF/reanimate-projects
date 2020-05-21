module A2048.Board where

import Data.Monoid
import Data.Coerce

import Control.Monad.Writer.CPS
import Control.Monad.State
import Control.Monad.Reader
import Control.Lens

import Graphics.SvgTree
import Reanimate

import A2048.Tile
import A2048.Config

type GameBoard = [[Int]]

newtype SeqAnim = SeqAnim { unwrapSeqAnim :: Animation }

instance Semigroup SeqAnim where
  SeqAnim a1 <> SeqAnim a2 = SeqAnim (a1 `seqA` a2)

instance Monoid SeqAnim where
  mempty = SeqAnim (staticFrame 0 None)

newtype ParAnim = ParAnim { unwrapParAnim :: Animation }

instance Semigroup ParAnim where
  ParAnim a1 <> ParAnim a2 = ParAnim (a1 `parA` a2)

instance Monoid ParAnim where
  mempty = ParAnim (staticFrame 0 None)

type GameT a m x = ReaderT BoardConfig (WriterT a (StateT GameBoard m)) x
type Game x = GameT SeqAnim Identity x

type IsAnim a = Coercible Animation a
type Monad2048 a m =
  ( IsAnim a, MonadWriter a m
  , MonadState GameBoard m
  , MonadReader BoardConfig m)

emptyBoard :: Monad2048 a m => m ()
emptyBoard = do
  w <- asks (view boardWidth)
  h <- asks (view boardHeight)
  put $ replicate h $ replicate w 0

tellA :: (IsAnim a, MonadWriter a m) => Animation -> m ()
tellA = tell . coerce

simultaneously :: Monad m => WriterT ParAnim m a -> WriterT SeqAnim m a
simultaneously m = writerT $ fmap parToSeq <$> runWriterT m
  where parToSeq (ParAnim a) = SeqAnim a

snapshot :: Monad2048 a m => m SVG
snapshot = do
  a <- asks (view tileSize)
  r <- asks (view tileRadius)
  g <- asks (view boardGapSize)
  w <- fromIntegral <$> asks (view boardWidth)
  h <- fromIntegral <$> asks (view boardHeight)
  let bw = w * a + (w + 2) * g
  let bh = h * a + (h + 2) * g
  grid <- get
  bg <- asks (view boardFillColour)
  let boardRect = roundedRect bw bh r & fillColor .~ Last (Just bg)
  mkGroup . (boardRect :) <$> sequenceA
    [ translate x y <$> if l == 0 then emptyTile else tile l
    | let sx = (w - 1) / 2
    , let sy = (h - 1) / 2
    , (m, row) <- zip [0 :: Int .. ] grid
    , let y = (sy - fromIntegral m) * (a + g)
    , (n, l) <- zip [0 :: Int .. ] row
    , let x = (fromIntegral n - sx) * (a + g) ]

hold :: Monad2048 a m => Double -> m ()
hold t = tellA . staticFrame t =<< snapshot

gameAnimation :: BoardConfig -> Game a -> Animation
gameAnimation cfg
  = coerce . snd
  . flip evalState []
  . runWriterT
  . flip runReaderT cfg
  . (emptyBoard *>)
