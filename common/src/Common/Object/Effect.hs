{-|
Module      : Common.Object.Effect
Description : Effects on objects.
Copyright   : (c) Xie Ruifeng, 2020
License     : AGPL-3
Maintainer  : krantz.xrf@outlook.com
Stability   : experimental
Portability : portable
-}
{-# LANGUAGE BlockArguments  #-}
{-# LANGUAGE TemplateHaskell #-}
module Common.Object.Effect
  (
  -- * Partial Boxes
  -- ** Data Type, Accessor
    Box(..)
  , boxWidth
  , boxHeight
  , boxStart
  , boxEnd
  , boxStroke
  , boxColour
  -- ** Scene API
  , oBoxNewOver
  , oBoxNewOverMany
  , oBoxGrow
  , oBoxErase
  -- * Primitives
  , partialBox
  ,oWiggle) where

import Common.Linear (Linear (lerp))
import Data.Maybe    (fromJust)
import Linear        (V2 (V2))

import Common.Animation.Effects
import Common.Object.Transform  (readGroupTrans)

import Control.Lens
import Control.Monad

import Codec.Picture.Types
import Graphics.SvgTree.Types
import Reanimate
import Reanimate.Scene

data VertexInfo = VertexInfo
  { vertexPosition :: V2 Coord
  , overallLength  :: Coord
  , lastLength     :: Coord
  } deriving (Show, Eq)

-- |Draw a partial box, provided a starting point and an ending point.
partialBox :: Coord -> Coord -> Double -> Double -> SVG
partialBox w h rs rt = withFillOpacity 0 tree where
  vertices = [V2 (-w') h', V2 w' h', V2 w' (-h'), V2 (-w') (-h')]
  info = zipWith3 VertexInfo
    (cycle vertices)
    (scanl (+) 0 (cycle [w, h]))
    (cycle [w, h])
  rest@(p0 : _) = dropWhile ((s >=) . overallLength . snd) $ zip info (tail info)
  x0r = (s - overallLength (fst p0)) / lastLength (fst p0)
  x0 = uncurry (lerp x0r) $ bimap vertexPosition vertexPosition p0
  (body, pt : _) = span ((t >=) . overallLength . snd) rest
  intermediate = map (vertexPosition . fst) (drop 1 body)
  xtr = (t - overallLength (fst pt)) / lastLength (fst pt)
  xt = uncurry (lerp xtr) $ bimap vertexPosition vertexPosition pt
  preXt = vertexPosition (fst pt)
  tailPath = if overallLength (fst pt) > s then [preXt, xt] else [xt]
  paths = MoveTo OriginAbsolute [x0]
        : map (LineTo OriginAbsolute . pure) (intermediate ++ tailPath)
  tree = pathTree defaultSvg{ _pathDefinition = paths }
  s = rs * (2 * w + 2 * h)
  t = rt * (2 * w + 2 * h)
  w' = w / 2
  h' = h / 2

-- |A potentialy partial box.
data Box = Box
  { _boxWidth  :: Coord
  , _boxHeight :: Coord
  , _boxStart  :: Double
  , _boxEnd    :: Double
  , _boxStroke :: Coord
  , _boxColour :: PixelRGBA8
  } deriving stock (Show, Eq)

makeLenses 'Box

instance Renderable Box where
  toSVG (Box w h s t width c)
    = withStrokeColorPixel c
    $ withStrokeWidth width
    $ partialBox w h s t

boxMargin :: Coord
boxMargin = 0.2

boxStrokeWidth :: Coord
boxStrokeWidth = 0.03

-- |Create a new box around the target.
oBoxNewOver :: Object s a -> PixelRGBA8 -> Scene s (Object s Box)
oBoxNewOver target c = do
  w <- oRead target oBBWidth
  h <- oRead target oBBHeight
  p <- oRead target oTranslate
  res <- oNew (Box (w + boxMargin) (h + boxMargin) 0 0 boxStrokeWidth c)
  oModify res (oTranslate .~ p)
  pure res

newtype BB a = BB (a, a, a, a) deriving stock Show

instance (Ord a, Num a) => Semigroup (BB a) where
  BB (x1, y1, w1, h1) <> BB (x2, y2, w2, h2) = BB (x, y, w, h) where
    x = min x1 x2
    y = min y1 y2
    w = max (x1 + w1) (x2 + w2) - x
    h = max (y1 + h1) (y2 + h2) - y

readRealBox :: Object s a -> Scene s (BB Double)
readRealBox obj = do
  V2 x0 y0 <- oRead obj oTranslate
  (x, y, w, h) <- oRead obj oBB
  pure (BB (x0 + x, y0 + y, w, h))

readWidthHeight :: Traversable t => t (Object s a) -> Scene s (Double, Double)
readWidthHeight xs = do
  BB (_, _, w, h) <- fromJust . foldMap Just <$> mapM readRealBox xs
  pure (w, h)

-- |Create a new box around a target group.
oBoxNewOverMany :: Traversable t => t (Object s a) -> PixelRGBA8 -> Scene s (Object s Box)
oBoxNewOverMany targets c = do
  (w, h) <- readWidthHeight targets
  p <- readGroupTrans targets
  res <- oNew (Box (w + boxMargin) (h + boxMargin) 0 0 boxStrokeWidth c)
  oModify res (oTranslate .~ p)
  pure res

-- |Grow the 'Box'.
oBoxGrow :: Duration -> Object s Box -> Scene s ()
oBoxGrow dt b = oShow b *> oTween b dt \t' -> let t = t' / dt in
  oValue %~ (boxStart .~ 0.5 + 0.6 * t) . (boxEnd .~ 0.5 + 1.6 * t)

-- |Erase the 'Box'.
oBoxErase :: Duration -> Object s Box -> Scene s ()
oBoxErase dt b = do
  oTween b dt $ \t' -> let t = t' / dt in
    oValue %~ (boxStart .~ 0.5 + 1.6 * t) . (boxEnd .~ 1.5 + 0.6 * t)
  oHide b

-- |Wiggle the 'Object'.
oWiggle :: Traversable t => t (Object s a) -> Scene s ()
oWiggle xs = do
  c <- readGroupTrans xs
  forM_ xs \x -> do
    p <- oRead x oTranslate
    oShowWith x (applyE (aroundE (c - p) (highlightE 0.2)) . staticFrame 1)
