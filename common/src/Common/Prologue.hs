{-|
Module      : Common.Prologue
Description : Common prologue animations.
Copyright   : (c) Xie Ruifeng, 2020
License     : AGPL-3
Maintainer  : krantz.xrf@outlook.com
Stability   : experimental
Portability : portable
-}
{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE OverloadedStrings #-}
module Common.Prologue (prologue) where

import Control.Lens
import Control.Monad

import Reanimate
import Reanimate.Scene

import Common.Object.Transform
import Common.Object.Types
import Common.SVG
import Graphics.SvgTree

normaliseTextPaths :: SVG -> SVG
normaliseTextPaths = mapTree (fillRule ?~ FillEvenOdd) . mapSvgPaths counterclockwiseLR

-- |Common prologue animation.
prologue :: Scene s ()
prologue = do
  chi <- oNewCentered $ normaliseTextPaths $ toSVG $ TeX "\\textbf{因真理·得自由·以幸福}"
  eng <- oNewCentered $ toSVG $ TeX "Freedom through Truth for Happiness"
  oModify chi (oTranslateY %~ (+ 0.5))
  oModify eng (oTranslateY %~ (- 0.5))
  oModify eng (oScale %~ (* 0.6))
  oShowWith chi (oStagger $ adjustDuration 4 . mapA (adjustStrokeWidth (* 0.6)) . oDraw)
  oShowWith eng (oStagger $ adjustDuration 2 . oDraw)
  wait 1
  waitOn $ forM_ [chi, eng] (fork . (`oHideWith` oFadeOut))
