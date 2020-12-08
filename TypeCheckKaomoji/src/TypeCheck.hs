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

import Data.Foldable        (Foldable (toList))
import Data.Functor         (($>))
import Data.Functor.Compose (Compose (..))
import Data.Maybe           (catMaybes, mapMaybe)
import Data.Monoid          (Last (..))
import GHC.Exts             (IsString (..))
import Linear.V2            (V2 (..))

import Common.Animation.Effects (addWhiteBkg)
import Common.HexColour         (rgba)
import Common.Linear            (Linear (lerp))

import Codec.Picture.Types
import Graphics.SvgTree
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
  = mapM oNewCentered . centerAsGroup
  . fmap (withFillColorPixel [rgba|000|] . withStrokeWidth 0 . withFillOpacity 1)
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

collectOld :: [Trans a b] -> [a]
collectOld = mapMaybe $ \case
  New _    -> Nothing
  Vanish x -> Just x
  x :=> _  -> Just x

type GroupAlignment t t' s a b = t (Object s a) -> t' (Object s b) -> Scene s ()

readCenterX :: Traversable t => t (Object s a) -> Scene s Double
readCenterX xs = do
  xMin <- minimum <$> mapM (`oRead` oLeftX) xs
  xMax <- maximum <$> mapM (`oRead` oRightX) xs
  pure ((xMin + xMax) / 2)

readCenterY :: Traversable t => t (Object s a) -> Scene s Double
readCenterY xs = do
  yMin <- minimum <$> mapM (`oRead` oBottomY) xs
  yMax <- maximum <$> mapM (`oRead` oTopY) xs
  pure ((yMin + yMax) / 2)

leftAligned :: (Traversable t, Traversable t') => GroupAlignment t t' s a b
leftAligned base xs = do
  xMin <- minimum <$> mapM (`oRead` oLeftX) base
  xOrig <- minimum <$> mapM (`oRead` oLeftX) xs
  let deltaX = xMin - xOrig
  mapM_ (`oModify` (oTranslateX %~ (+ deltaX))) xs

rightAligned :: (Traversable t, Traversable t') => GroupAlignment t t' s a b
rightAligned base xs = do
  xMax <- maximum <$> mapM (`oRead` oRightX) base
  xOrig <- maximum <$> mapM (`oRead` oRightX) xs
  let deltaX = xMax - xOrig
  mapM_ (`oModify` (oTranslateX %~ (+ deltaX))) xs

xCentered :: (Traversable t, Traversable t') => GroupAlignment t t' s a b
xCentered base xs = do
  xCenter <- readCenterX base
  xOrig <- readCenterX xs
  let deltaX = xCenter - xOrig
  mapM_ (`oModify` (oTranslateX %~ (+ deltaX))) xs

bottomAligned :: (Traversable t, Traversable t') => GroupAlignment t t' s a b
bottomAligned base xs = do
  yMin <- minimum <$> mapM (`oRead` oBottomY) base
  yOrig <- minimum <$> mapM (`oRead` oBottomY) xs
  let deltaY = yMin - yOrig
  mapM_ (`oModify` (oTranslateY %~ (+ deltaY))) xs

topAligned :: (Traversable t, Traversable t') => GroupAlignment t t' s a b
topAligned base xs = do
  yMax <- maximum <$> mapM (`oRead` oTopY) base
  yOrig <- maximum <$> mapM (`oRead` oTopY) xs
  let deltaY = yMax - yOrig
  mapM_ (`oModify` (oTranslateY %~ (+ deltaY))) xs

yCentered :: (Traversable t, Traversable t') => GroupAlignment t t' s a b
yCentered base xs = do
  yCenter <- readCenterY base
  yOrig <- readCenterY xs
  let deltaY = yCenter - yOrig
  mapM_ (`oModify` (oTranslateY %~ (+ deltaY))) xs

applyAlignment :: (Traversable t, Traversable t')
               => [GroupAlignment t t' s a b]
               -> t (Object s a)
               -> t' (Object s b)
               -> Scene s (t' (Object s b))
applyAlignment align base xs = mapM_ (($ xs) . ($ base)) align $> xs

transformCodeChunks :: [GroupAlignment [] (Compose [] (Trans (Object s a))) s a SVG]
                    -> [Trans (Object s a) T.Text]
                    -> Scene s [Object s SVG]
transformCodeChunks align xs
  = waitOn $ codeChunks (Compose xs)
  >>= applyAlignment align (collectOld xs)
  >>= fmap catMaybes . mapM (fork . \case
    New b     -> oShowWith b oFadeIn $> Just b
    Vanish a  -> oHideWith a oFadeOut $> Nothing
    a :=> b -> Just b <$ do
      V2 x0 y0 <- oRead a oTranslate
      V2 x  y  <- oRead b oTranslate
      ctx <- oRead a oContext
      oModify b (oContext .~ ctx)
      let xt t = lerp t x0 x
      let yt t = lerp t y0 y
      oTween a 1 (\t -> oTranslate .~ V2 (xt t) (yt t))
      oHide a
      oShow b
  ) . getCompose

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
  ~xs@[_, d3l, _, d3r, _, _, _] <- transformCodeChunks [yCentered, xCentered]
    [lp :=> "(", "(", d3 :=> ".", ") ", d1 :=> "(.) ", d2 :=> "(.)", rp :=> ")"]
  tweenColours
    [ (d3l, [rgba|C19C00|])
    , (d3r, [rgba|C19C00|]) ]
  waitOn $ mapM_ (fork . moveVertical 2) xs
  waitOn $ mapM_ (fork . (`oHideWith` oFadeOut)) xs
  wait 1
