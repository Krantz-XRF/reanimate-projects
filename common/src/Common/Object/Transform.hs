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
  , oNewGroupScaled
  , oNewCentered
  -- * Alignment
  -- | Alignment controls how the objects in the next key frame is aligned according to
  --   those objects from the current frame.
  , GroupAlignment
  , applyAlignment
  -- ** Group Modifier
  , scaled
  , allMargins
  , withMargin
  -- ** X Alignment
  , leftAligned
  , rightAligned
  , xCentered
  , placedBelow
  -- ** Y Alignment
  , bottomAligned
  , topAligned
  , yCentered
  -- * Key Frame Animations
  , Trans(.., (:=>))
  , transOrigin
  , transformObject'
  , transformObject
  , dupFromObject
  -- * Object duplication
  , dupObject
  -- * Primitives
  , readGroupTrans
  ) where

import Control.Lens
import Control.Monad

import Data.Functor.Compose (Compose (..))
import Data.Maybe           (mapMaybe)
import GHC.Exts             (IsString (..))
import Linear.V2            (V2 (..))
import Linear.Vector        ((*^))

import Graphics.SvgTree.Types (Coord)
import Reanimate
import Reanimate.Scene

-- |Duplicate the object, make it independent from the original.
dupObject :: Renderable a => Object s a -> Scene s (Object s a)
dupObject x = do
  d <- oRead x id
  y <- oNew (view oValue d)
  oModify y (const d)
  pure y

-- |Description for key frame animations.
--
-- __Note__:
--
-- * 'IsString' instance provided here for conveniently creating new objects.
-- * The following objects are collected as the base for alignment:
--
--     * Objects that vanished, or
--     * Source objects for transformations
--
-- * The following objects are collected as the target for alignment:
--
--     * Objects newly created, or
--     * Target objects for transformations
data Trans a b
  -- |Object newly created in this frame.
  = New b
  -- |Previous object vanishing in this frame.
  | Vanish a
  -- |Object transformaton in this frame.
  | [a] ::=> b
  deriving stock (Show, Functor, Foldable, Traversable)

-- |Short cut for single object transformaton in this frame.
pattern (:=>) :: a -> b -> Trans a b
pattern a :=> b = [a] ::=> b

instance IsString b => IsString (Trans a b) where
  fromString = New . fromString

-- |Traverse the origins of the 'Trans'.
transOrigin :: Traversal (Trans a b) (Trans a' b) a a'
transOrigin _ (New b)    = pure (New b)
transOrigin f (Vanish a) = Vanish <$> f a
transOrigin f (a ::=> b) = (::=>) <$> traverse f a <*> pure b

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
oNewGroup :: forall a t s . (Traversable t, GroupRender a) => t a -> Scene s (t (Object s SVG))
oNewGroup = mapM oNewCentered . renderGroup

-- |Create a group of objects, scaled as a group, translation of each lifted to 'Object' level.
oNewGroupScaled :: forall a t s . (Traversable t, GroupRender a)
                => Double -> t a -> Scene s (t (Object s SVG))
oNewGroupScaled c = mapM (oNewCentered . scale c) . renderGroup

collectOld :: [Trans a b] -> Compose [] [] a
collectOld = Compose . mapMaybe \case
  New _    -> Nothing
  Vanish x -> Just [x]
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

-- |Placed below the alignment base.
placedBelow :: (Traversable t, Traversable t') => GroupAlignment t t' s a b
placedBelow bs xs = do
  minY <- minimum <$> mapM (`oRead` oBottomY) bs
  maxY <- maximum <$> mapM (`oRead` oTopY) xs
  let deltaY = minY - maxY
  mapM_ (\x -> oModify x $ oTranslateY %~ (+ deltaY)) xs

-- |Aligned according to the bottom boundary (minimum Y).
bottomAligned :: (Traversable t, Traversable t') => GroupAlignment t t' s a b
bottomAligned = alignedOf oBottomY minimum oTranslateY

-- |Aligned according to the top boundary (minimum Y).
topAligned :: (Traversable t, Traversable t') => GroupAlignment t t' s a b
topAligned = alignedOf oTopY maximum oTranslateY

-- |Aligned according to the vertical center line (average of min/max Y).
yCentered :: (Traversable t, Traversable t') => GroupAlignment t t' s a b
yCentered = centeredOf readCenterY oTranslateY

-- |'Traversal' of all four margins.
allMargins :: Traversal (a, a, a, a) (b, b, b, b) a b
allMargins f (x, y, z, w) = (,,,) <$> f x <*> f y <*> f z <*> f w

-- |Set margin.
withMargin :: (Traversable t, Traversable t') => Coord -> GroupAlignment t t' s a SVG
withMargin d _ = mapM_ (`oModify` (oMargin . allMargins .~ d))

-- |Scaled by a factor.
scaled :: (Traversable t, Traversable t') => Double -> GroupAlignment t t' s a SVG
scaled d _ xs = forM_ xs \x -> do
  oModify x (oTranslate %~ (d *^))
  oModify x (oValue %~ scale d)

-- |Apply some 'GroupAlignment's.
applyAlignment :: (Traversable t, Traversable t')
               => [GroupAlignment t t' s a b]
               -> t (Object s a)
               -> t' (Object s b)
               -> Scene s ()
applyAlignment align base xs = mapM_ (($ xs) . ($ base)) align

-- |Key frame animation, with automatic creation for new objects.
--
-- __Warning__: if alignment base (see 'Trans' for details) is empty, then all the
-- alignment functions provided in this module would crash your program. In this
-- case, custom (ad-hoc) alignment functions might be useful. Or, to specify a
-- custom alignment base @bs@, use @const (alignment bs)@ instead of @alignment@.
--
-- __Note__: if the target object utilise @OverloadedStrings@ via 'IsString', the type
-- becomes ambiguous. Use @TypeApplications@ for this situation:
--
-- > transformObject' @ObjType alignments transformations
transformObject' :: GroupRender t
                 => [GroupAlignment (Compose [] []) (Compose [] (Trans (Object s a))) s a SVG]
                 -> [Trans (Object s a) t]
                 -> Scene s [Object s SVG]
transformObject' align xs
  = oNewGroup (Compose xs)
  >>= applyAlignment' (collectOld xs)
  >>= transformObjectRaw . getCompose . fmap pure
  where applyAlignment' base ys = ys <$ applyAlignment align base ys

transformObjectRaw :: [Trans (Object s a) [Object s b]] -> Scene s [Object s b]
transformObjectRaw = waitOn . fmap concat . mapM (fork . \case
    New b    -> b <$ mapM_ (`oShowWith` oFadeIn) b
    Vanish a -> [] <$ oHideWith a oFadeOut
    a ::=> b -> b <$ do
      p0 <- readGroupTrans a
      p  <- readGroupTrans b
      ctx <- oRead (head a) oContext
      mapM_ (`oModify` (oContext .~ ctx)) b
      waitOn $ forM_ a $ \i ->
        fork $ oTween i 1 $ \t ->
          oTranslate %~ (+ t *^ (p - p0))
      mapM_ oHide a
      mapM_ oShow b
  )

-- |Key frame animation.
--
-- __Warning__: if alignment base (see 'Trans' for details) is empty, then all the
-- alignment functions provided in this module would crash your program. In this
-- case, custom (ad-hoc) alignment functions might be useful. Or, to specify a
-- custom alignment base @bs@, use @const (alignment bs)@ instead of @alignment@.
transformObject :: [Trans (Object s a) [Object s b]] -> Scene s ()
transformObject = void . transformObjectRaw

-- |Key frame animation, but without hiding the original.
dupFromObject :: Renderable a => [Trans (Object s a) [Object s b]] -> Scene s ()
dupFromObject = mapMOf (traverse . transOrigin) dupObject >=> transformObject

-- |Read the overall translation of an 'Object' group.
readGroupTrans :: Traversable t => t (Object s a) -> Scene s (V2 Double)
readGroupTrans = sequence . (V2 <$> readCenterX <*> readCenterY)
