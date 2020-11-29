{-|
Module      : TypeCheck
Description : Aniamtions for type-checking.
Copyright   : (c) Xie Ruifeng, 2020
License     : AGPL-3
Maintainer  : krantz.xrf@outlook.com
Stability   : experimental
Portability : portable
-}
{-# LANGUAGE OverloadedStrings #-}
module TypeCheck (typeCheckAnim) where

import qualified Data.Text as T

import Control.Lens

import Data.Functor         (($>))
import Data.Functor.Compose (Compose (..))
import Data.Maybe           (catMaybes)
import Linear.V2            (V2 (..))

import Common.Animation.Effects (addWhiteBkg)

import Data.Foldable
import Reanimate
import Reanimate.LaTeX
import Reanimate.Scene

centerAsGroup :: (Functor t, Foldable t) => t SVG -> t SVG
centerAsGroup xs = let g = mkGroup (toList xs) in fmap (centerUsing g) xs

oNewCentered :: SVG -> Scene s (Object s SVG)
oNewCentered s = do
  let (x, y, w, h) = boundingBox s
  o <- oNew (center s)
  oModify o (oTranslate .~ V2 (x + w / 2) (y + h / 2))
  pure o

codeChunks :: Traversable t => t T.Text -> Scene s (t (Object s SVG))
codeChunks
  = mapM oNewCentered . centerAsGroup . fmap (withFillColor "black" . withStrokeWidth 0)
  . latexCfgChunksTrans texCfg code
  where texCfg = TexConfig
          { texConfigEngine = XeLaTeX
          , texConfigHeaders =
            [ "\\usepackage{fontspec}"
            , "\\setmonofont{JetBrains Mono}[Contextuals=Alternate]" ]
          , texConfigPostScript = []
          }
        code x = "\\texttt{" <> x <> "}"

showCodeChunks :: [T.Text] -> Scene s [Object s SVG]
showCodeChunks xs = waitOn $ codeChunks xs >>= mapM (fork . \x -> oShowWith x oFadeIn $> x)

data Trans a b
  = New b
  | Vanish a
  | Trans a b
  deriving stock (Show, Functor, Foldable, Traversable)

transformCodeChunks :: Renderable a => [Trans (Object s a) T.Text] -> Scene s [Object s SVG]
transformCodeChunks xs
  = waitOn $ codeChunks (Compose xs) >>= fmap catMaybes . mapM (fork . \case
    New b     -> oShowWith b oFadeIn $> Nothing
    Vanish a  -> oHideWith a oFadeOut $> Nothing
    Trans a b -> Just b <$ do
      V2 x0 y0 <- oRead a oTranslate
      V2 x  y  <- oRead b oTranslate
      let lerp t s v = (1 - t) * s + t * v
      let xt t = lerp t x0 x
      let yt t = lerp t y0 y
      oTween a 1 (\t -> oTranslate .~ V2 (xt t) (yt t))
  ) . getCompose

typeCheckAnim :: Animation
typeCheckAnim = mapA addWhiteBkg $ scene $ do
  ~[lp, d1, d3, d2, rp] <- showCodeChunks ["(", "(.)", " . ", "(.)", ")"]
  xs <- transformCodeChunks
    [Trans lp "(", New "(", Trans d3 ".", New ") ", Trans d1 "(.) ", Trans d2 "(.)", Trans rp ")"]
  mapM_ (`oHideWith` oFadeOut) xs
