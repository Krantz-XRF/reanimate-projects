{-|
Module      : TypeCheck
Description : Animations for type-checking.
Copyright   : (c) Xie Ruifeng, 2020
License     : AGPL-3
Maintainer  : krantz.xrf@outlook.com
Stability   : experimental
Portability : portable
-}
{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module TypeCheck (typeCheckAnim) where

import qualified Data.Text as T

import Control.Lens

import Data.Functor (($>))
import Data.Monoid  (Last (..))

import Common.Animation.Effects (addWhiteBkg)
import Common.HexColour         (FromRGBA8, rgba)
import Common.Linear            (Linear (lerp))
import Common.Object.Transform
import Common.Object.Types

import Codec.Picture.Types  hiding (Traversal)
import Common.Object.Effect
import Data.Foldable
import Data.Functor.Compose
import Graphics.SvgTree
import Reanimate
import Reanimate.Scene

yellow, red, blue, green :: FromRGBA8 c => c
yellow = [rgba|C19C00|]
red    = [rgba|E74856|]
blue   = [rgba|3A96DD|]
green  = [rgba|00c19c|]

showCodeChunks :: [CodeChunk] -> Scene s [Object s SVG]
showCodeChunks expr = waitOn $ oNewGroup expr >>= mapM (fork . \x -> oShowWith x oFadeIn $> x)

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

data EquationF s a = Equation
  { equationLhs :: [a]
  , equalSign   :: a
  , equationRhs :: [a]
  } deriving stock (Functor, Foldable, Traversable)

type Equation s = EquationF s (Object s SVG)

allMargins :: Traversal (a, a, a, a) (b, b, b, b) a b
allMargins f (x, y, z, w) = (,,,) <$> f x <*> f y <*> f z <*> f w

newEquation :: [CodeChunk] -> [CodeChunk] -> Scene s (Equation s)
newEquation lhs rhs = do
  eq <- oNewGroupScaled 0.3 (Equation lhs " ~ " rhs)
  mapM_ (`oModify` (oMargin . allMargins .~ 0.3)) eq
  pure eq

showEquation :: Renderable a
             => [CodeChunk] -> [Object s a]
             -> [CodeChunk] -> [Object s a]
             -> [Equation s -> Scene s ()]
             -> Scene s (Equation s)
showEquation lhs lhsOrig rhs rhsOrig f = do
  eq <- newEquation lhs rhs
  mapM_ ($ eq) f
  forM_ eq (`oModify` (oContext .~ transColour 1 green))
  tweenColours $ map (, green) (lhsOrig ++ rhsOrig)
  dupFromObject
    [ lhsOrig ::=> equationLhs eq
    , rhsOrig ::=> equationRhs eq ]
  equalSign eq `oShowWith` oFadeIn
  waitOn $ do
    fork $ tweenColoursWith (1 -) $ map (, green) (lhsOrig ++ rhsOrig)
    fork $ tweenColoursWith (1 -) $ map (, green) (toList eq)
  pure eq

alignEquation :: Equation s -> Equation s -> Scene s ()
alignEquation e1 e2 = do
  x1 <- oRead (equalSign e1) oTranslateX
  x2 <- oRead (equalSign e2) oTranslateX
  let dx = x1 - x2
  forM_ e2 (`oModify` (oTranslateX %~ (+ dx)))

oBoxNewOverArrow :: [Object s a] -> Int -> PixelRGBA8 -> Scene s (Object s Box)
oBoxNewOverArrow arr arrIdx c = do
  box <- oBoxNewOverMany arr c
  y <- oRead (arr !! arrIdx) oTranslateY
  oModify box (oTranslateY .~ y)
  pure box

oBoxGrowAll :: Traversable t => Duration -> t (Object s Box) -> Scene s ()
oBoxGrowAll dt = waitOn . mapM_ (fork . oBoxGrow dt)

oFadeHideAll :: Traversable t => t (Object s a) -> Scene s ()
oFadeHideAll = waitOn . mapM_ (fork . (`oHideWith` oFadeOut))

transColour :: Double -> PixelRGBA8 -> SVG -> Tree
transColour t c s = mapTree (withFillColorPixel (lerp t (getFillColour s) c)) s

tweenColour :: Duration -> Signal -> (Object s a, PixelRGBA8) -> Scene s ()
tweenColour dt f (x, c) = oTween x dt (\t -> oContext .~ transColour (f t) c)

blink :: Object s SVG -> PixelRGBA8 -> Scene s ()
blink x c = do
  tweenColour 0.3 id (x, c)
  tweenColour 0.2 (1 -) (x, c)
  tweenColour 0.3 id (x, c)

revertBlink :: Object s SVG -> PixelRGBA8 -> Scene s ()
revertBlink x c = tweenColour 0.2 (1 -) (x, c)

tweenColours :: [(Object s a, PixelRGBA8)] -> Scene s ()
tweenColours = tweenColoursWith id

tweenColoursWith :: Signal -> [(Object s a, PixelRGBA8)] -> Scene s ()
tweenColoursWith f = waitOn . mapM_ (fork . tweenColour 1 f)

typeCheckAnim :: Animation
typeCheckAnim = mapA addWhiteBkg $ scene $ do
  -- main composition illustration
  ~[lp, d1, d3, d2, rp] <- showCodeChunks ["(", "(.)", " . ", "(.)", ")"]
  tweenColours
    [ (d1, red)
    , (d2, blue)
    , (d3, yellow) ]
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
  ~expr@[_, d3l, _, d3r, _, _, _] <- transformObject' @CodeChunk [yCentered, xCentered]
    [lp :=> "(", "(", d3 :=> ".", ") ", d1 :=> "(.) ", d2 :=> "(.)", rp :=> ")"]
  tweenColours
    [ (d3l, yellow)
    , (d3r, yellow) ]
  -- composition move up
  waitOn $ mapM_ (fork . moveVertical 2) expr
  -- show signature yellow, red, blue.
  sig1 <- showDotSig yellow "xyz" expr
  sig2 <- showDotSig red    "abc" sig1
  sig3 <- showDotSig blue   "pqr" sig2
  -- all move left, make room for type equations
  waitOn $ forM_ (Compose [sig1, sig2, sig3]) $ \x ->
    fork $ oTween x 1 (\t -> oTranslateX %~ subtract (2.5 * t))
  -- box '(y -> z)' and '(b -> c) -> (a -> b) -> a -> c' with red
  byz <- oBoxNewOverArrow (sig1 & drop 2 & take 5) 2 red
  bsig2 <- oBoxNewOverArrow (sig2 & drop 2) 5 red
  oBoxGrowAll 1 [byz, bsig2]
  waitOn $ do
    fork $ (sig1 !! 4) `blink` red
    fork $ (sig2 !! 7) `blink` red
  wait 1
  -- new equation 'y ~ b -> c'
  eq1 <- showEquation
    ["y"] (sig1 & drop 3 & take 1)
    ["b -> c"] (sig2 & drop 3 & take 3)
    [yCentered sig1, mapM_ (`oModify` (oTranslateX %~ (+ 3)))]
  -- new equation 'z ~ (a -> b) -> a -> c'
  eq2 <- showEquation
    ["z"] (sig1 & drop 5 & take 1)
    ["(a -> b) -> a -> c"] (sig2 & drop 8)
    [alignEquation eq1, placedBelow eq1]
  -- revert red color on arrows
  waitOn $ do
    fork $ (sig1 !! 4) `revertBlink` red
    fork $ (sig2 !! 7) `revertBlink` red
  -- box '(x -> y)' and '(q -> r) -> (p -> q) -> p -> r' with blue
  bxy <- oBoxNewOverArrow (sig1 & drop 8 & take 5) 2 blue
  bsig3 <- oBoxNewOverArrow (sig3 & drop 2) 5 blue
  oBoxGrowAll 1 [bxy, bsig3]
  waitOn $ do
    fork $ (sig1 !! 10) `blink` blue
    fork $ (sig3 !! 7) `blink` blue
  wait 1
  -- new equation 'x ~ q -> r'
  eq3 <- showEquation
    ["x"] (sig1 & drop 9 & take 1)
    ["q -> r"] (sig3 & drop 3 & take 3)
    [alignEquation eq2, placedBelow eq2]
  -- new equation 'y ~ (p -> q) -> p -> r'
  eq4 <- showEquation
    ["y"] (sig1 & drop 11 & take 1)
    ["(p -> q) -> p -> r"] (sig3 & drop 8)
    [alignEquation eq3, placedBelow eq3]
  -- revert blue color on arrows
  waitOn $ do
    fork $ (sig1 !! 10) `revertBlink` blue
    fork $ (sig3 !! 7) `revertBlink` blue
  -- hide everything
  waitOn $ do
    fork $ oFadeHideAll $ Compose [sig1, sig2, sig3, expr]
    fork $ oFadeHideAll $ Compose [eq1, eq2, eq3, eq4]
    fork $ oFadeHideAll [bxy, byz, bsig2, bsig3]
  wait 1
