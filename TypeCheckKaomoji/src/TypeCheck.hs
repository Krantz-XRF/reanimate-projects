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
{-# LANGUAGE QuasiQuotes       #-}
module TypeCheck (typeCheckAnim) where

import qualified Data.Text as T

import Control.Lens

import Data.Foldable        (Foldable (toList))
import Data.Functor         (($>))
import Data.Functor.Compose (Compose (..))
import Data.Maybe           (catMaybes, fromMaybe)
import Data.Monoid          (Last (..))
import GHC.Exts             (IsString (..))
import Linear.V2            (V2 (..))

import Common.Animation.Effects (addWhiteBkg)
import Common.HexColour

import Codec.Picture.Types
import Graphics.SvgTree
import Reanimate
import Reanimate.LaTeX
import Reanimate.Scene

pattern Colour8 :: Pixel8 -> Pixel8 -> Pixel8 -> Pixel8 -> Texture
pattern Colour8 r g b a = ColorRef (PixelRGBA8 r g b a)

withTexture :: Texture -> SVG -> SVG
withTexture t = mapTree (fillColor .~ Last (Just t))

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
  = mapM oNewCentered . centerAsGroup
  . fmap (withTexture [rgba|000|] . withStrokeWidth 0 . withFillOpacity 1)
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
  | a :=> b
  deriving stock (Show, Functor, Foldable, Traversable)

instance IsString b => IsString (Trans a b) where
  fromString = New . fromString

transformCodeChunks :: Renderable a => [Trans (Object s a) T.Text] -> Scene s [Object s SVG]
transformCodeChunks xs
  = waitOn $ codeChunks (Compose xs) >>= fmap catMaybes . mapM (fork . \case
    New b     -> oShowWith b oFadeIn $> Just b
    Vanish a  -> oHideWith a oFadeOut $> Nothing
    a :=> b -> Just b <$ do
      V2 x0 y0 <- oRead a oTranslate
      V2 x  y  <- oRead b oTranslate
      ctx <- oRead a oContext
      oModify b (oContext .~ ctx)
      let lerp t s v = (1 - t) * s + t * v
      let xt t = lerp t x0 x
      let yt t = lerp t y0 y
      oTween a 1 (\t -> oTranslate .~ V2 (xt t) (yt t))
      oHide a
      oShow b
  ) . getCompose

typeCheckAnim :: Animation
typeCheckAnim = mapA addWhiteBkg $ scene $ do
  ~[lp, d1, d3, d2, rp] <- showCodeChunks ["(", "(.)", " . ", "(.)", ")"]
  let lerpColour t ~(Colour8 r g b a) s =
        let Colour8 r0 g0 b0 a0 = fromMaybe [rgba|000|] (getLast (view fillColor s))
            c = Colour8 (lerp t r0 r) (lerp t g0 g) (lerp t b0 b) (lerp t a0 a)
        in withTexture c s
      lerp t s v = round $ (1 - t) * fromIntegral s + t * fromIntegral v
  let tweenColour (x, c) = fork $ oTween x 1 (\t -> oContext .~ lerpColour t c)
  let tweenColours = waitOn . mapM_ tweenColour
  tweenColours
    [ (d1, [rgba|E74856|])
    , (d2, [rgba|3A96DD|])
    , (d3, [rgba|C19C00|]) ]
  ~xs@[_, d3l, _, d3r, _, _, _] <- transformCodeChunks
    [lp :=> "(", "(", d3 :=> ".", ") ", d1 :=> "(.) ", d2 :=> "(.)", rp :=> ")"]
  tweenColours
    [ (d3l, [rgba|C19C00|])
    , (d3r, [rgba|C19C00|]) ]
  waitOn $ mapM_ (\x -> fork $ oHideWith x oFadeOut) xs
  wait 1
