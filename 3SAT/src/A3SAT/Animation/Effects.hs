{-|
Module      : A3SAT.Animation.Effects
Description : Animation effects for 3-SAT problem.
Copyright   : (c) Xie Ruifeng, 2020
License     : AGPL-3-or-later
Maintainer  : krantz.xrf@outlook.com
Stability   : experimental
Portability : portable
-}
module A3SAT.Animation.Effects where

import qualified Data.Text as T

import Control.Lens
import Control.Monad.Reader.Class
import Data.Foldable
import Data.Maybe

import Graphics.SvgTree
import Reanimate

import A3SAT.Animation.Config
import A3SAT.Animation.Primitives
import A3SAT.Expression
import A3SAT.Expression.Lens

import Common.Animation.Effects

-- |All animations of 3-SAT problems run in a 'Monad3SAT'.
type Monad3SAT m = MonadReader Anim3SATConfig m

-- |Convert a 'Monad3SAT' action to an animation.
exprAnimation :: Anim3SATConfig -> (Anim3SATConfig -> a) -> a
exprAnimation = (&)

-- |Prepare an expression.
prepareExpr :: Monad3SAT m => [T.Text] -> Expression -> m (LExpression SVG)
prepareExpr xs e
  = reader $ \cfg@Anim3SATConfig{..} -> e
  & colourVar id (getVarColours cfg)
  . fmap (fillColor .~ pure _operatorColour)
  . showSvg' xs

-- |Render an expression as a static frame.
snapshotExpr :: Monad3SAT m => [T.Text] -> Expression -> m SVG
snapshotExpr xs = fmap exprSvg . prepareExpr xs

-- |Hold an expression static for some 'Time'.
holdExpr :: Monad3SAT m => [T.Text] -> Expression -> Time -> m Animation
holdExpr xs e t = staticFrame t <$> snapshotExpr xs e

-- |Hold an expression static for some 'Time', with some 'ColourStrategy'.
holdExprWith :: Monad3SAT m => ColourStrategy -> [T.Text] -> Expression -> Time -> m Animation
holdExprWith strat xs e = local (colourStrategy .~ strat) . holdExpr xs e

-- |Highlight the variables with configured colours.
highlightVars :: Monad3SAT m => [T.Text] -> Expression -> m Animation
highlightVars xs e = do
  e' <- prepareExpr xs e
  Anim3SATConfig{..} <- ask
  let lerp t c = lerpColour _defaultTextColour c t
  pure $ animate $ \t -> exprSvg (colourVar id (map (lerp t) _varColours) e')

-- |Apply the 'True'/'False' values, show the process.
applyBoolValue :: Monad3SAT m => [Bool] -> LExpression SVG -> m Animation
applyBoolValue bs expr = reader $ \Anim3SATConfig{..} ->
  let tfColour b = if b then _trueColour else _falseColour
      extendExpr = fmap (\(v, svg) -> (v, v, svg, pause 0))
      shrinkExpr = fmap (\(_, v, svg, _) -> (v, svg))
      handleTerm s = s & primaryLabel %~ handleLabel s
      handleLabel s (v, v', svg, _) = (v, v', svg, colourAnim v v' (fmap (view _3) s))
      colourAnim v v' s
        | v /= v' = parallel $ fmap (colourTrans $ tfColour $ fromJust v') s
        | otherwise = pause 0
      colourTrans c svg = animate $ \t -> svg & fillColor %~ fmap (\c' -> lerpColour c' c t)
      go (anim, e) (n, b) =
        let e' = extendExpr e & over asPoS handleTerm . evalExprIdx _2 . appNewVal
            asPoS = asProduct . transParen . asSum
            appNewVal = mapVar _2 undefined (== n) (\_ _ -> Just b)
            animVar = parallel (fmap (view _4) e')
            valueChanged (x, y, _, _) = x /= y
            changedExpr = e' ^.. asProduct . filtered (valueChanged . view primaryLabel)
            rectAnim x =
              let c = pure $ tfColour $ fromJust $ x ^. primaryLabel . _2
                  rect = set strokeColor c
                       $ withFillOpacity 0
                       $ withStrokeWidth (defaultStrokeWidth * 1.5)
                       $ aroundCenter (scaleXY 1.05 1.3)
                       $ mkBoundingRect
                       $ fmap (view _3) x
              in animate (`partialSvg` rect)
            animRect = signalA (curveS 2) $ parallel (map rectAnim changedExpr)
        in (anim `andThen` animVar `andThen` animRect, shrinkExpr e')
  in fst $ foldl' go (pause 0, fmap (Nothing, ) expr) (zip [0 :: Word ..] bs)
