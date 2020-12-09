{-|
Module      : TypeCheck
Description : Animations for type-checking.
Copyright   : (c) Xie Ruifeng, 2020
License     : AGPL-3
Maintainer  : krantz.xrf@outlook.com
Stability   : experimental
Portability : portable
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module TypeCheck (typeCheckAnim) where

import Control.Lens

import Data.Functor (($>))
import Data.Monoid  (Last (..))

import Common.Animation.Effects (addWhiteBkg)
import Common.HexColour         (rgba)
import Common.Linear            (Linear (lerp))
import Common.Object.Transform
import Common.Object.Types

import Codec.Picture.Types
import Graphics.SvgTree
import Reanimate
import Reanimate.Scene

showCodeChunks :: [CodeChunk] -> Scene s [Object s SVG]
showCodeChunks xs = waitOn $ oNewGroup xs >>= mapM (fork . \x -> oShowWith x oFadeIn $> x)

getFillColour :: SVG -> PixelRGBA8
getFillColour = maybe [rgba|000|] fromColorRef . getLast . view fillColor
  where fromColorRef ~(ColorRef p) = p

moveVertical :: Double -> Object s a -> Scene s ()
moveVertical d x = oTween x 1 (\t -> oTranslateY %~ (t * d +))

typeCheckAnim :: Animation
typeCheckAnim = mapA addWhiteBkg $ scene $ do
  ~[lp, d1, d3, d2, rp] <- showCodeChunks ["(", "(.)", " . ", "(.)", ")"]
  let transColour t c s = mapTree (withFillColorPixel (lerp t (getFillColour s) c)) s
  let tweenColour (x, c) = fork $ oTween x 1 (\t -> oContext .~ transColour t c)
  let tweenColours = waitOn . mapM_ tweenColour
  tweenColours
    [ (d1, [rgba|E74856|])
    , (d2, [rgba|3A96DD|])
    , (d3, [rgba|C19C00|]) ]
  oPopBubble 2 d3
    [ LeftAligned $ AnyRenderable $ TeX "函数复合，二元中缀运算符。"
    , LeftAligned $ AnyRenderable $ HaskellBubble
      "(.) :: (b -> c) -> (a -> b) -> a -> c\n\
      \(.) f g = \\x -> f (g x)\n\
      \\n\
      \infixr 9 ." ]
  ~xs@[_, d3l, _, d3r, _, _, _] <- transformObject @CodeChunk [yCentered, xCentered]
    [lp :=> "(", "(", d3 :=> ".", ") ", d1 :=> "(.) ", d2 :=> "(.)", rp :=> ")"]
  tweenColours
    [ (d3l, [rgba|C19C00|])
    , (d3r, [rgba|C19C00|]) ]
  waitOn $ mapM_ (fork . moveVertical 2) xs
  waitOn $ mapM_ (fork . (`oHideWith` oFadeOut)) xs
  wait 1
