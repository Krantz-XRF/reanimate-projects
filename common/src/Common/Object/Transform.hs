{-|
Module      : Common.Object.Transform
Description : Key frame animation for object transformation.
Copyright   : (c) Xie Ruifeng, 2020
License     : AGPL-3
Maintainer  : krantz.xrf@outlook.com
Stability   : experimental
Portability : portable
-}
{-# LANGUAGE BlockArguments #-}
module Common.Object.Transform
  (
  -- * Renderable Objects
  -- | 'SceneRender' is used for procedural 'Object' creation in place of 'Renderable'.
    GroupRender(..)
  , oNewGroup
  , oNewCentered
  -- * Alignment
  -- | Alignment controls how the objects in the next key frame is aligned according to
  --   those objects from the current frame.
  , GroupAlignment
  -- ** X Alignment
  , leftAligned
  , rightAligned
  , xCentered
  -- ** Y Alignment
  , bottomAligned
  , topAligned
  , yCentered
  -- * Key Frame Animations
  , Trans(.., (:=>))
  , transformObject
  ) where

import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as N

import Common.Linear (Linear (lerp))

import Control.Lens
import Control.Monad

import Data.Functor         (($>))
import Data.Functor.Compose (Compose (..))
import Data.Maybe           (catMaybes, mapMaybe)
import GHC.Exts             (IsString (..))
import Linear.V2            (V2 (..))

import Reanimate
import Reanimate.Scene

-- |Description for key frame animations.
--
-- __Note__:
--
-- * 'IsString' instance provided here for conveniently creating new objects.
-- * The following objects are collected as the base for alignment:
--
--     * Objects that vanished, or
--     * Target objects for transformations
--
-- * The following objects are collected as the target for alignment:
--
--     * Objects newly created, or
--     * Source objects for transformations
data Trans a b
  -- |Object newly created in this frame.
  = New b
  -- |Previous object vanishing in this frame.
  | Vanish a
  -- |Object transformaton in this frame.
  | (NonEmpty a) ::=> b
  deriving stock (Show, Functor, Foldable, Traversable)

-- |Short cut for single object transformaton in this frame.
pattern (:=>) :: a -> b -> Trans a b
pattern a :=> b = (a :| []) ::=> b

instance IsString b => IsString (Trans a b) where
  fromString = New . fromString

-- |Allow rendering for groups of objects.
class GroupRender a where
  -- |Render a group of objects to a group of SVGs.
  -- The group structure should be taken into consideration while rendering.
  -- e.g. Group of 'T.Text' can be rendered using 'Reanimate.LaTeX.latexChunks'.
  renderGroup :: Traversable t => t a -> t SVG

-- |New object, with its translation lifted to 'Object' level.
oNewCentered :: SVG -> Scene s (Object s SVG)
oNewCentered s = do
  let (x, y, w, h) = boundingBox s
  o <- oNew (center s)
  oModify o (oTranslate .~ V2 (x + w / 2) (y + h / 2))
  pure o

-- |Create a group of objects, translation of each lifted to 'Object' level.
oNewGroup :: (Traversable t, GroupRender a) => t a -> Scene s (t (Object s SVG))
oNewGroup = mapM oNewCentered . renderGroup

collectOld :: [Trans a b] -> Compose [] NonEmpty a
collectOld = Compose . mapMaybe \case
  New _    -> Nothing
  Vanish x -> Just (x :| [])
  x ::=> _ -> Just x

-- |Group alignment, given a base, align the second parameter according to the base.
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
  aCenter <- readCenter base
  aOrig <- readCenter xs
  let delta = aCenter - aOrig
  mapM_ (`oModify` (setPos %~ (+ delta))) xs

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
-- __Warning__: if alignment base (see 'Trans' for details) is empty, then all the
-- alignment functions provided in this module would crash your program. In this
-- case, custom (ad-hoc) alignment functions might be useful. Or, to specify a
-- custom alignment base @bs@, use @const (alignment bs)@ instead of @alignment@.
--
-- __Note__: if the target object utilise @OverloadedStrings@ via 'IsString', the type
-- becomes ambiguous. Use @TypeApplications@ for this situation:
--
-- > transformObject @ObjType alignments transformations
transformObject :: GroupRender t
                => [GroupAlignment (Compose [] NonEmpty) (Compose [] (Trans (Object s a))) s a SVG]
                -> [Trans (Object s a) t]
                -> Scene s [Object s SVG]
transformObject align xs
  = waitOn $ oNewGroup (Compose xs)
  >>= applyAlignment align (collectOld xs)
  >>= fmap catMaybes . mapM (fork . \case
    New b     -> oShowWith b oFadeIn $> Just b
    Vanish a  -> oHideWith a oFadeOut $> Nothing
    a ::=> b -> Just b <$ do
      V2 x0 y0 <- readGroupTrans a
      V2 x  y  <- oRead b oTranslate
      ctx <- oRead (N.head a) oContext
      oModify b (oContext .~ ctx)
      waitOn $ forM_ a $ \i ->
        fork $ oTween i 1 $ \t ->
          oTranslate .~ V2 (lerp t x0 x) (lerp t y0 y)
      mapM_ oHide a
      oShow b
  ) . getCompose

readGroupTrans :: Traversable t => t (Object s a) -> Scene s (V2 Double)
readGroupTrans = sequence . (V2 <$> readCenterX <*> readCenterY)
