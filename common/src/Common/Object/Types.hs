{-|
Module      : Common.Object.Types
Description : Some useful object types.
Copyright   : (c) Xie Ruifeng, 2020
License     : AGPL-3
Maintainer  : krantz.xrf@outlook.com
Stability   : experimental
Portability : portable
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Common.Object.Types where

import qualified Data.Text as T

import Control.Lens  ((.~))
import Data.Foldable (Foldable (toList))
import GHC.Exts      (IsString (..))
import Linear        (V2 (V2))

import Common.HexColour        (rgba)
import Common.Object.Transform (SceneRender (..))

import Reanimate
import Reanimate.LaTeX
import Reanimate.Scene

-- |New object, with its translation lifted to 'Object' level.
oNewCentered :: SVG -> Scene s (Object s SVG)
oNewCentered s = do
  let (x, y, w, h) = boundingBox s
  o <- oNew (center s)
  oModify o (oTranslate .~ V2 (x + w / 2) (y + h / 2))
  pure o

-- |Center a group of objects.
centerAsGroup :: (Functor t, Foldable t) => t SVG -> t SVG
centerAsGroup xs = let g = mkGroup (toList xs) in fmap (centerUsing g) xs

-- |Code chunks typeset by XeLaTeX in type writer font.
newtype CodeChunk = CodeChunk { unwrapCodeChunk :: T.Text }
  deriving stock (Show, Eq, Ord)
  deriving newtype (IsString)

instance SceneRender CodeChunk where
  renderGroup = mapM oNewCentered . centerAsGroup
    . fmap (withFillColorPixel [rgba|000|] . withStrokeWidth 0 . withFillOpacity 1)
    . latexCfgChunksTrans texCfg code . fmap unwrapCodeChunk
    where
      texCfg = TexConfig
        { texConfigEngine = XeLaTeX
        , texConfigHeaders =
          [ "\\usepackage{fontspec}"
          , "\\setmonofont{JetBrains Mono}[Contextuals=Alternate]" ]
        , texConfigPostScript = []
        }
      code x = "\\texttt{" <> x <> "}"

