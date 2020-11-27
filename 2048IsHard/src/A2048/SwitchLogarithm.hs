{-|
Module      : A2048.SwitchLogarithm
Description : Animation for switching config 'A2048.Config.useLogarithm'.
Copyright   : (c) Xie Ruifeng, 2020
License     : AGPL-3
Maintainer  : krantz.xrf@outlook.com
Stability   : experimental
Portability : portable
-}
module A2048.SwitchLogarithm where

import Control.Lens
import Control.Monad.Reader.Class

import Reanimate
import Reanimate.Morph.Common
import Reanimate.Morph.Linear
import Reanimate.Transition

import A2048.Board
import A2048.Config
import A2048.Tile

import Common.Animation.Effects

-- |Create animation for each non-empty grids with the provided function.
animateGrids :: Monad2048 m => (Time -> Int -> Game SVG) -> m Animation
animateGrids f = do
  bd <- local (tileShowLabel .~ False) snapshot
  a <- mkPure (foreachNonEmptyGrid . f)
  pure $ animate (mkGroup . (bd :) . a)

-- |Animation for the board from 'Plain' to 'Exponent'.
switchPlainExpo :: Monad2048 m => m Animation
switchPlainExpo = animateGrids $ \t n -> do
  l0 <- tileLabel n
  l1 <- local (tileLabelMode .~ Exponent) (tileLabel n)
  let linearDup = linear{morphObjectCorrespondence = dupObjectCorrespondence}
  pure (morph linearDup l0 l1 t)

-- |Highlight the exponents in 'Exponent'.
expoHighlight :: Monad2048 m => m Animation
expoHighlight = animateGrids $ \t n -> do
  l1 <- local (tileLabelMode .~ Exponent) (tileLabel n)
  let two : (mkGroup -> expo) = [f s | (f, _, s) <- svgGlyphs l1]
  pure $ mkGroup [two, aroundCenterE (highlightE 0.2) 1 t expo]

-- |Animation for fading the base 2 in 'Exponent'.
expoFadeTwo :: Monad2048 m => m Animation
expoFadeTwo = animateGrids $ \t n -> do
  l1 <- local (tileLabelMode .~ Exponent) (tileLabel n)
  let two : expos = [f s | (f, _, s) <- svgGlyphs l1]
  pure $ mkGroup (fadeOutE 1 t two : expos)

-- |Animation for moving the exponent down, forming a 'Logarithm'.
expoMoveDown :: Monad2048 m => m Animation
expoMoveDown = animateGrids $ \t n -> do
  l1 <- local (tileLabelMode .~ Exponent) (tileLabel n)
  l2 <- local (tileLabelMode .~ Logarithm) (tileLabel n)
  let _ : (mkGroup -> expo) = [f s | (f, _, s) <- svgGlyphs l1]
  pure (lerpSVG expo l2 t)

-- |Animation for the board from 'Exponent' to 'Logarithm'.
switchExpoLog :: Monad2048 m => m Animation
switchExpoLog = overlapT 0.5 (const id) <$> expoFadeTwo <*> expoMoveDown
