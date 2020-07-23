{-|
Module      : A2048.Text
Description : Text labels for narrations.
Copyright   : (c) Xie Ruifeng, 2020
License     : AGPL-3
Maintainer  : krantz.xrf@outlook.com
Stability   : experimental
Portability : portable
-}
{-# LANGUAGE OverloadedStrings #-}
module A2048.Text where

import qualified Data.Text as T

import Reanimate
import Reanimate.LaTeX

-- |Plain text labels, with Source Han fonts.
label :: T.Text -> SVG
label = center
  . withStrokeWidth 0 . withFillOpacity 1
  . withFillColor "black"
  . ctexWithHeaders
  [ "\\setCJKmainfont{Source Han Serif SC}"
  , "\\setCJKsansfont{Source Han Sans SC}" ]
