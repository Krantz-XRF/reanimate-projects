{-|
Module      : A3SAT.Animation.Effects
Description : Animation effects for 3-SAT problem.
Copyright   : (c) Xie Ruifeng, 2020
License     : AGPL-3
Maintainer  : krantz.xrf@outlook.com
Stability   : experimental
Portability : portable
-}
module A3SAT.Animation.Effects where

import qualified Data.Text as T

import Control.Lens
import Control.Monad.Reader.Class

import Graphics.SvgTree
import Reanimate

import A3SAT.Animation.Config
import A3SAT.Animation.Primitives
import A3SAT.Expression

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
