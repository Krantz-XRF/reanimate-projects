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
module Common.Prologue where

import Control.Lens
import Control.Monad

import Reanimate
import Reanimate.Scene

import Common.Object.Transform
import Common.Object.Types

-- |Common prologue animation.
prologue :: Scene s ()
prologue = do
  chi <- oNewCentered $ toSVG $ TeX "\\textbf{因真理·得自由·以幸福}"
  eng <- oNewCentered $ toSVG $ TeX "Freedom through Truth for Happiness"
  oModify chi (oTranslateY %~ (+ 0.5))
  oModify eng (oTranslateY %~ (- 0.5))
  oModify eng (oScale %~ (* 0.6))
  oShowWith chi (oStagger oDraw)
  oShowWith eng (oStagger oDraw)
  wait 1
  waitOn $ forM_ [chi, eng] (fork . (`oHideWith` oFadeOut))
