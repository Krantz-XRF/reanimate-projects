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

import qualified Data.Text as T

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

showDotSig :: PixelRGBA8 -> String -> [Object s a] -> Scene s [Object s SVG]
showDotSig col vars pos = do
  let [a, b, c] = map (CodeChunk . T.singleton) vars
  ~sig1@(f:_) <- oNewGroupScaled @CodeChunk 0.3
    [ "(.)", " :: "
    , "(", b, " -> ", c, ")", " -> "
    , "(", a, " -> ", b, ")", " -> "
    , a, " -> ", c ]
  xCentered pos sig1
  placedBelow pos sig1
  oModify f (oContext .~ mapTree (withFillColorPixel col))
  waitOn $ mapM_ (fork . (`oShowWith` oFadeIn)) sig1
  pure sig1

typeCheckAnim :: Animation
typeCheckAnim = mapA addWhiteBkg $ scene $ do
  -- main composition illustration
  ~[lp, d1, d3, d2, rp] <- showCodeChunks ["(", "(.)", " . ", "(.)", ")"]
  let transColour t c s = mapTree (withFillColorPixel (lerp t (getFillColour s) c)) s
  let tweenColour (x, c) = fork $ oTween x 1 (\t -> oContext .~ transColour t c)
  let tweenColours = waitOn . mapM_ tweenColour
  tweenColours
    [ (d1, [rgba|E74856|])
    , (d2, [rgba|3A96DD|])
    , (d3, [rgba|C19C00|]) ]
  -- show description for '(.)'
  oPopBubble 2 d3
    [ LeftAligned $ AnyRenderable $ TeX "\\sf 函数复合，二元中缀运算符："
    , Centered $ AnyRenderable $ TeX "$(f \\circ g)(x) = f(g(x))$"
    , LeftAligned $ AnyRenderable $ TeX "\\sf Haskell定义如下："
    , LeftAligned $ AnyRenderable $ HaskellBubble
      "(.) :: (b -> c) -> (a -> b) -> a -> c\n\
      \(.) f g = \\x -> f (g x)\n\
      \\n\
      \infixr 9 ." ]
  -- infix style -> function style
  ~xs@[_, d3l, _, d3r, _, _, _] <- transformObject @CodeChunk [yCentered, xCentered]
    [lp :=> "(", "(", d3 :=> ".", ") ", d1 :=> "(.) ", d2 :=> "(.)", rp :=> ")"]
  tweenColours
    [ (d3l, [rgba|C19C00|])
    , (d3r, [rgba|C19C00|]) ]
  -- composition move up
  waitOn $ mapM_ (fork . moveVertical 2) xs
  -- show signature yellow, red, blue.
  sig1 <- showDotSig [rgba|C19C00|] "xyz" xs
  sig2 <- showDotSig [rgba|E74856|] "abc" sig1
  sig3 <- showDotSig [rgba|3A96DD|] "pqr" sig2
  -- hide everything
  waitOn $ mapM_ (fork . (`oHideWith` oFadeOut)) $ concat
    [sig1, sig2, sig3, xs]
  wait 1
