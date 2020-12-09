{-|
Module      : Common.Object.Transform
Description : Key frame animation for object transformation.
Copyright   : (c) Xie Ruifeng, 2020
License     : AGPL-3
Maintainer  : krantz.xrf@outlook.com
Stability   : experimental
Portability : portable
-}
module Common.Object.Transform
  (
  -- * Renderable Objects
  -- | 'SceneRender' is used for procedural 'Object' creation in place of 'Renderable'.
    SceneRender(..)
  -- * Alignment
  -- | Alignment controls how the objects in the next key frame is aligned according to
  --   those objects from the current frame.
  -- ** X Alignment
  , leftAligned
  , rightAligned
  , xCentered
  -- ** Y Alignment
  , bottomAligned
  , topAligned
  , yCentered
  -- * Key Frame Animations
  , Trans(..)
  , transformObject
  ) where

import Common.Linear (Linear (lerp))

import Control.Lens

import Data.Functor         (($>))
import Data.Functor.Compose (Compose (..))
import Data.Maybe           (catMaybes, mapMaybe)
import GHC.Exts             (IsString (..))
import Linear.V2            (V2 (..))

import Reanimate
import Reanimate.Scene

-- |Description for key frame animations.
--
-- Notes:
-- * 'IsString' instance provided here for conveniently creating new objects.
-- * The following objects are collected as the base for alignment:
--   * Objects that vanished, or
--   * Target objects for transformations
-- * The following objects are collected as the target for alignment:
--   * Objects newly created, or
--   * Source objects for transformations
data Trans a b
  -- |Object newly created in this frame.
  = New b
  -- |Previous object vanishing in this frame.
  | Vanish a
  -- |Object transformaton in this frame.
  | a :=> b
  deriving stock (Show, Functor, Foldable, Traversable)

instance IsString b => IsString (Trans a b) where
  fromString = New . fromString

-- |Procedural rendering for groups of objects.
class SceneRender a where
  -- |Render a group of objects in a 'Scene'.
  -- The group structure should be taken into consideration while rendering.
  -- e.g. Group of 'T.Text' can be rendered using 'Reanimate.LaTeX.latexChunks'.
  renderGroup :: Traversable t => t a -> Scene s (t (Object s SVG))

collectOld :: [Trans a b] -> [a]
collectOld = mapMaybe $ \case
  New _    -> Nothing
  Vanish x -> Just x
  x :=> _  -> Just x

type GroupAlignment t t' s a b = t (Object s a) -> t' (Object s b) -> Scene s ()

readCenterOf :: Traversable t
             => Getter (ObjectData a) Double
             -> Getter (ObjectData a) Double
             -> t (Object s a)
             -> Scene s Double
readCenterOf lb ub xs = do
  aMin <- minimum <$> mapM (`oRead` lb) xs
  aMax <- maximum <$> mapM (`oRead` ub) xs
  pure ((aMin + aMax) / 2)

readCenterX :: Traversable t => t (Object s a) -> Scene s Double
readCenterX = readCenterOf oLeftX oRightX

readCenterY :: Traversable t => t (Object s a) -> Scene s Double
readCenterY = readCenterOf oBottomY oTopY

centeredOf :: (Traversable t, Traversable t')
           => (forall x f . Traversable f => f (Object s x) -> Scene s Double)
           -> (forall x . Setter' (ObjectData x) Double)
           -> GroupAlignment t t' s a b
centeredOf readCenter setPos base xs = do
  xCenter <- readCenter base
  xOrig <- readCenter xs
  let deltaX = xCenter - xOrig
  mapM_ (`oModify` (setPos %~ (+ deltaX))) xs

alignedOf :: (Traversable t, Traversable t')
          => (forall x . Getter (ObjectData x) Double)
          -> (forall f . Traversable f => f Double -> Double)
          -> Setter' (ObjectData b) Double
          -> GroupAlignment t t' s a b
alignedOf alignStd collect trans base xs = do
  aMax  <- collect <$> mapM (`oRead` alignStd) base
  aOrig <- collect <$> mapM (`oRead` alignStd) xs
  let delta = aMax - aOrig
  mapM_ (`oModify` (trans %~ (+ delta))) xs

-- |Aligned according to the left boundary (minimum X).
leftAligned :: (Traversable t, Traversable t') => GroupAlignment t t' s a b
leftAligned = alignedOf oLeftX minimum oTranslateX

-- |Aligned according to the right boundary (maximum X).
rightAligned :: (Traversable t, Traversable t') => GroupAlignment t t' s a b
rightAligned = alignedOf oRightX maximum oTranslateY

-- |Aligned according to the horizontal center line (average of min/max X).
xCentered :: (Traversable t, Traversable t') => GroupAlignment t t' s a b
xCentered = centeredOf readCenterX oTranslateX

-- |Aligned according to the bottom boundary (minimum Y).
bottomAligned :: (Traversable t, Traversable t') => GroupAlignment t t' s a b
bottomAligned = alignedOf oBottomY minimum oTranslateY

-- |Aligned according to the top boundary (minimum Y).
topAligned :: (Traversable t, Traversable t') => GroupAlignment t t' s a b
topAligned = alignedOf oTopY maximum oTranslateY

-- |Aligned according to the vertical center line (average of min/max Y).
yCentered :: (Traversable t, Traversable t') => GroupAlignment t t' s a b
yCentered = centeredOf readCenterY oTranslateY

applyAlignment :: (Traversable t, Traversable t')
               => [GroupAlignment t t' s a b]
               -> t (Object s a)
               -> t' (Object s b)
               -> Scene s (t' (Object s b))
applyAlignment align base xs = mapM_ (($ xs) . ($ base)) align $> xs

-- |Key frame animation.
--
-- Warning: if alignment base (see 'Trans' for details) is empty, then all the
-- alignment functions provided in this module would crash your program. In this
-- case, custom (ad-hoc) alignment functions might be useful.
--
-- Note: if the target object utilise @OverloadedStrings@ via 'IsString', the type
-- becomes ambiguous. Use @TypeApplications@ for this situation:
-- > transformObject @ObjType alignments transformations
transformObject :: SceneRender t
                => [GroupAlignment [] (Compose [] (Trans (Object s a))) s a SVG]
                -> [Trans (Object s a) t]
                -> Scene s [Object s SVG]
transformObject align xs
  = waitOn $ renderGroup (Compose xs)
  >>= applyAlignment align (collectOld xs)
  >>= fmap catMaybes . mapM (fork . \case
    New b     -> oShowWith b oFadeIn $> Just b
    Vanish a  -> oHideWith a oFadeOut $> Nothing
    a :=> b -> Just b <$ do
      V2 x0 y0 <- oRead a oTranslate
      V2 x  y  <- oRead b oTranslate
      ctx <- oRead a oContext
      oModify b (oContext .~ ctx)
      let xt t = lerp t x0 x
      let yt t = lerp t y0 y
      oTween a 1 (\t -> oTranslate .~ V2 (xt t) (yt t))
      oHide a
      oShow b
  ) . getCompose
