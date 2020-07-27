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

import Control.Lens

import Graphics.SvgTree
import Reanimate
import Reanimate.LaTeX

-- |Plain text labels, with Source Sans Pro, Source Han fonts.
label :: T.Text -> SVG
label = colourLabel (mkColor "black")

-- |Coloured text labels, with Source Sans Pro, Source Han fonts.
colourLabel :: Texture -> T.Text -> SVG
colourLabel c = center
  . set fillColor (pure c)
  . ctexWithHeaders
  [ "\\setsansfont{Source Sans Pro}"
  , "\\setCJKmainfont{Source Han Serif SC}"
  , "\\setCJKsansfont{Source Han Sans SC}" ]
