{-|
Module      : A2048.SwitchLogarithm
Description : Animation for switching config 'useLogarithm'.
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

import A2048.Config
import A2048.Tile
import A2048.Board

import Anim.Common

-- |Animation for the board from 2-4-8-16 to 1-2-3-4.
switchLog :: Monad2048 m => m Animation
switchLog = do
  bd <- local (tileShowLabel .~ False) snapshot
  let animateGrids f = do
        a <- mkPure (foreachNonEmptyGrid . f)
        pure $ animate (mkGroup . (bd :) . a)
  toExpo <- animateGrids $ \t n -> do
    l0 <- tileLabel n
    l1 <- local (tileLabelMode .~ Exponent) (tileLabel n)
    pure (morph linear l0 l1 t)
  fadeTwo <- animateGrids $ \t n -> do
    l1 <- local (tileLabelMode .~ Exponent) (tileLabel n)
    let two : expos = [f s | (f, _, s) <- svgGlyphs l1]
    pure $ mkGroup (fadeOutE 1 t two : expos)
  moveExpo <- animateGrids $ \t n -> do
    l1 <- local (tileLabelMode .~ Exponent) (tileLabel n)
    l2 <- local (tileLabelMode .~ Logarithm) (tileLabel n)
    let _ : (mkGroup -> expos) = [f s | (f, _, s) <- svgGlyphs l1]
    pure (lerpSVG expos l2 t)
  pure (toExpo `seqA` fadeTwo `seqA` moveExpo)
