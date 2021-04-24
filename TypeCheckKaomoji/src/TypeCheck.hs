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
import Control.Monad

import Data.Functor (($>))

import Common.Animation.Effects (addWhiteBkg)
import Common.HexColour         (FromRGBA8, rgba)
import Common.Linear            (Linear (lerp))
import Common.Object.Effect
import Common.Object.Transform
import Common.Object.Types

import Codec.Picture.Types  hiding (Traversal)
import Data.Foldable
import Data.Functor.Compose
import Data.Functor.Product
import Graphics.SvgTree
import Reanimate
import Reanimate.Scene

yellow, red, blue, green :: FromRGBA8 c => c
yellow = [rgba|C19C00|]
red    = [rgba|E74856|]
blue   = [rgba|3A96DD|]
green  = [rgba|00c19c|]

showMathChunks :: [MathChunk] -> Scene s [Object s SVG]
showMathChunks expr = waitOn $ oNewGroup expr >>= mapM (fork . \x -> oShowWith x oFadeIn $> x)

getFillColour :: SVG -> PixelRGBA8
getFillColour = maybe [rgba|000|] fromColorRef . view fillColor
  where fromColorRef ~(ColorRef p) = p

moveVertical :: Double -> Object s a -> Scene s ()
moveVertical d x = oTween x 1 (\t -> oTranslateY %~ (t * d +))

showDotSig :: PixelRGBA8 -> String
           -> [[Object s SVG] -> Scene s ()]
           -> Scene s [Object s SVG]
showDotSig col vars pos = do
  let [a, b, c] = map (MathChunk . T.singleton) vars
  ~sig@(f:_) <- oNewGroupScaled @MathChunk 0.4
    [ "(.)", " : "
    , "(", b, " -> ", c, ")", " -> "
    , "(", a, " -> ", b, ")", " -> "
    , a, " -> ", c ]
  mapM_ ($ sig) pos
  oModify f (oContext .~ mapTree (withFillColorPixel col))
  waitOn $ mapM_ (fork . (`oShowWith` oFadeIn)) sig
  pure sig

data EquationF s a = Equation
  { equationLhs :: [a]
  , equalSign   :: a
  , equationRhs :: [a]
  } deriving stock (Functor, Foldable, Traversable)

type Equation s = EquationF s (Object s SVG)

newEquation :: [MathChunk] -> [MathChunk] -> Scene s (Equation s)
newEquation lhs rhs = do
  eq <- oNewGroupScaled 0.4 (Equation lhs " = " rhs)
  withMargin 0.3 [] eq
  pure eq

showEquation :: Renderable a
             => [MathChunk] -> [Object s a]
             -> [MathChunk] -> [Object s a]
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
  waitOn do
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

oFadeShowAll :: Traversable t => t (Object s a) -> Scene s ()
oFadeShowAll = waitOn . mapM_ (fork . (`oShowWith` oFadeIn))

oFadeHideAll :: Traversable t => t (Object s a) -> Scene s ()
oFadeHideAll = waitOn . mapM_ (fork . (`oHideWith` oFadeOut))

oFadePopUp :: Traversable t => Duration -> t (Object s a) -> Scene s ()
oFadePopUp dt xs = do
  oFadeShowAll xs
  wait dt
  oFadeHideAll xs

transColour :: Double -> PixelRGBA8 -> SVG -> Tree
transColour t c s = mapTree (withFillColorPixel (lerp t (getFillColour s) c)) s

tweenColour :: Duration -> Signal -> (Object s a, PixelRGBA8) -> Scene s ()
tweenColour dt f (x, c) = oTween x dt (\t -> oContext .~ transColour (f t) c)

blink :: Bool -> Int -> Object s SVG -> PixelRGBA8 -> Scene s ()
blink b n x c = do
  unless b $ tweenColour 0.2 (1 -) (x, c)
  replicateM_ n do
    tweenColour 0.3 id (x, c)
    tweenColour 0.2 (1 -) (x, c)
  when b $ tweenColour 0.3 id (x, c)

tweenColours :: [(Object s a, PixelRGBA8)] -> Scene s ()
tweenColours = tweenColoursWith id

tweenColoursWith :: Signal -> [(Object s a, PixelRGBA8)] -> Scene s ()
tweenColoursWith f = waitOn . mapM_ (fork . tweenColour 1 f)

typeCheckAnim :: Animation
typeCheckAnim = mapA addWhiteBkg $ scene do
  -- main composition illustration
  ~tgt@[lp, d1, d3, d2, rp] <- showMathChunks ["(", "(.)", " . ", "(.)", ")"]
  tweenColours
    [ (d1, red)
    , (d2, blue)
    , (d3, yellow) ]
  -- show description for '(.)'
  ~[def] <- oNewGroupScaled @MathChunk 0.6 ["(f . g)(x) = f(g(x))"]
  placedBelow tgt [def]
  ~[arrowUp] <- oNewGroupScaled @MathChunk 0.6 ["\\uparrow"]
  oModify arrowUp (oTranslateY .~ -1)
  oFadePopUp 2 [arrowUp, def]
  -- infix style -> function style
  ~expr@[lp', d3l, d3', d3r, d1', d2', rp'] <- transformObject' @MathChunk [yCentered, xCentered]
    [lp :=> "(", "(", d3 :=> ".", ")~", d1 :=> "(.)~", d2 :=> "(.)", rp :=> ")"]
  tweenColours
    [ (d3l, yellow)
    , (d3r, yellow) ]
  -- composition move up
  waitOn $ mapM_ (fork . moveVertical 2) expr
  -- show signature yellow, red, blue.
  sig1 <- showDotSig yellow "xyz" [placedBelow expr, xCentered expr]
  sig2 <- showDotSig red    "abc" [placedBelow sig1, leftAligned sig1]
  sig3 <- showDotSig blue   "pqr" [placedBelow sig2, leftAligned sig2]
  -- all move left, make room for type equations
  waitOn $ forM_ (Compose [sig1, sig2, sig3]) $ \x ->
    fork $ oTween x 1 (\t -> oTranslateX %~ subtract (2.5 * t))
  -- box '(y -> z)' and '(b -> c) -> (a -> b) -> a -> c' with red
  byz <- oBoxNewOverArrow (sig1 & drop 2 & take 5) 2 red
  bsig2 <- oBoxNewOverArrow (sig2 & drop 2) 5 red
  oBoxGrowAll 1 [byz, bsig2]
  waitOn do
    fork $ blink True 1 (sig1 !! 4) red
    fork $ blink True 1 (sig2 !! 7) red
  wait 1
  -- new equation 'y = b -> c'
  eq1 <- showEquation
    ["y"] (sig1 & drop 3 & take 1)
    ["b", " -> ", "c"] (sig2 & drop 3 & take 3)
    [yCentered sig1, mapM_ (`oModify` (oTranslateX %~ (+ 3)))]
  -- new equation 'z = (a -> b) -> a -> c'
  eq2 <- showEquation
    ["z"] (sig1 & drop 5 & take 1)
    ["(a -> ", "b", ") -> a -> ", "c"] (sig2 & drop 8)
    [alignEquation eq1, placedBelow eq1]
  -- revert red color on arrows
  waitOn do
    fork $ blink False 0 (sig1 !! 4) red
    fork $ blink False 0 (sig2 !! 7) red
  -- box '(x -> y)' and '(q -> r) -> (p -> q) -> p -> r' with blue
  bxy <- oBoxNewOverArrow (sig1 & drop 8 & take 5) 2 blue
  bsig3 <- oBoxNewOverArrow (sig3 & drop 2) 5 blue
  oBoxGrowAll 1 [bxy, bsig3]
  waitOn do
    fork $ blink True 1 (sig1 !! 10) blue
    fork $ blink True 1 (sig3 !! 7) blue
  wait 1
  -- new equation 'x = q -> r'
  eq3 <- showEquation
    ["x"] (sig1 & drop 9 & take 1)
    ["q -> r"] (sig3 & drop 3 & take 3)
    [alignEquation eq2, placedBelow eq2]
  -- new equation 'y = (p -> q) -> p -> r'
  eq4 <- showEquation
    ["y"] (sig1 & drop 11 & take 1)
    ["(", "p -> q", ")", " -> ", "p -> r"] (sig3 & drop 8)
    [alignEquation eq3, placedBelow eq3]
  -- revert blue color on arrows
  waitOn do
    fork $ blink False 0 (sig1 !! 10) blue
    fork $ blink False 0 (sig3 !! 7) blue
  -- box 'x -> z' with yellow, show ': x -> z' for title
  bxz <- oBoxNewOverArrow (sig1 & drop 14) 1 yellow
  ~title@(exprNewPos : xz@[xz_colon, xz_x, xz_arr, xz_z])
    <- oNewGroupScaled @MathChunk 0.7 ["((.)~(.)~(.))", ":", "x", " -> ", "z"]
  applyAlignment [yCentered] expr title
  oBoxGrow 1 bxz
  transformObject [expr ::=> [exprNewPos]]
  mapM_ oShow expr; oHide exprNewPos
  waitOn $ forM_ xz \i -> wait 0.5 >> fork do
    oModify i (oValue %~ withStrokeWidth defaultStrokeWidth)
    oShowWith i oDraw
    fork $ oTween i 0.5 \t ->
      oValue %~ withStrokeWidth (t * defaultStrokeWidth)
  -- hide all boxes
  oFadeHideAll [bxy, byz, bsig2, bsig3, bxz]
  -- move left, remove color in title, and hide dot-signatures
  waitOn do
    fork $ tweenColoursWith (1 -) $ map (, yellow) (expr & drop 1 & take 3)
    fork $ tweenColour 1 (1 -) (expr !! 4, red)
    fork $ tweenColour 1 (1 -) (expr !! 5, blue)
    fork $ oFadeHideAll (Compose [sig1, sig2, sig3])
    forM_ (Compose [sig1, sig2, sig3] `Pair` Compose [eq1, eq2, eq3, eq4])
      \x -> fork $ oTween x 1 (\t -> oTranslateX %~ subtract (t * 6))
  -- title back to infix notation
  ~[lp'', d1'', d3'', d2'', rp'', xz'] <- oNewGroupScaled @MathChunk 0.7
    ["(", "(.)", ".", "(.)", ")", ": x -> z"]
  let expr' = [lp'', d1'', d3'', d2'', rp'']
  yCentered expr (xz' : expr')
  transformObject
    [ lp' :=> [lp''], Vanish d3l, d3' :=> [d3''], Vanish d3r
    , d1' :=> [d1''], d2' :=> [d2''], rp' :=> [rp''], xz ::=> [xz']]
  mapM_ oShow xz; oHide xz'
  -- wiggle 'y' in eq1 & eq4
  waitOn do
    fork $ oWiggle (equationLhs eq1)
    fork $ oWiggle (equationLhs eq4)
  -- box rhs of eq1 & eq4
  beq1 <- oBoxNewOverMany (equationRhs eq1) red
  beq4 <- oBoxNewOverMany (equationRhs eq4) red
  oBoxGrowAll 1 [beq1, beq4]
  waitOn do
    fork $ blink True 1 (equationRhs eq1 !! 1) red
    fork $ blink True 1 (equationRhs eq4 !! 3) red
  -- new equation 'b = p -> q'
  eq5 <- showEquation
    ["b"] [head (equationRhs eq1)]
    ["p -> q"] [equationRhs eq4 !! 1]
    [yCentered eq1, mapM_ (`oModify` (oTranslateX %~ (+ 3)))]
  -- new equation 'c = p -> r'
  eq6 <- showEquation
    ["c"] [equationRhs eq1 !! 2]
    ["p -> r"] [equationRhs eq4 !! 4]
    [alignEquation eq5, placedBelow eq5]
  -- revert color on arrows & hide box
  waitOn do
    fork $ oFadeHideAll [beq1, beq4]
    fork $ blink False 0 (equationRhs eq1 !! 1) red
    fork $ blink False 0 (equationRhs eq4 !! 3) red
  -- hide eq1 & eq4 (no longer needed)
  oFadeHideAll (eq1 `Pair` eq4)
  waitOn $ forM_ (eq2 `Pair` eq3) \x ->
    fork $ oTween x 1 (\t -> oTranslateY %~ (+ t))
  wait 1
  -- highlight rhs of eq5 & eq6, highlight b & c
  let eq2Rhs = equationRhs eq2
  waitOn do
    fork $ tweenColours $ map (, red) (eq2Rhs !! 1 : equationRhs eq5)
    fork $ tweenColours $ map (, blue) (eq2Rhs !! 3 : equationRhs eq6)
  wait 1
  -- substitute b & c into eq2
  eq2Rhs' <- waitOn do
    fork $ oFadeHideAll $
      equalSign eq5 : equalSign eq6 : equationLhs eq5 ++ equationLhs eq6
    fork $ transformObject' @MathChunk
      [withMargin 0.3, scaled 0.4, leftAligned, const (yCentered eq2)]
      [ head eq2Rhs :=> "(a -> "
      , equationRhs eq5 ::=> "p -> q"
      , eq2Rhs !! 2 :=> ") -> a -> "
      , equationRhs eq6 ::=> "p -> r"
      , Vanish (eq2Rhs !! 1), Vanish (eq2Rhs !! 3) ]
  let eq2' = eq2{ equationRhs = eq2Rhs' }
  oFadeHideAll (eq5 `Pair` eq6)
  -- revert color on 'p -> q' & 'p -> r' back to black
  waitOn do
    fork $ blink False 0 (eq2Rhs' !! 1) red
    fork $ blink False 0 (eq2Rhs' !! 3) blue
  wait 1
  -- highlight rhs of eq2 & eq3, highlight z & x in title
  waitOn do
    fork $ tweenColours $ map (, blue) (xz_z : equationRhs eq2')
    fork $ tweenColours $ map (, red) (xz_x : equationRhs eq3)
  wait 1
  -- substitute z & x into title
  title' <- oNewGroupScaled @MathChunk 0.5
    [ "((.) . (.))", ":", "(", "q -> r", ")"
    , " -> ", "(a -> p -> q) -> a -> p -> r" ]
  yCentered title title'
  waitOn do
    fork $ oFadeHideAll [xz_x, xz_z]
    wait 0.5
    fork $ oFadeHideAll $
      equalSign eq2' : equalSign eq3 : equationLhs eq2' ++ equationLhs eq3
    fork $ transformObject
      [ expr' ::=> [head title']
      , xz_colon :=> [title' !! 1]
      , equationRhs eq3 ::=> [title' !! 3]
      , xz_arr :=> [title' !! 5]
      , equationRhs eq2' ::=> [title' !! 6] ]
  waitOn do
    fork $ oFadeShowAll [title' !! 2, title' !! 4]
    fork $ tweenColoursWith (1 -) [(title' !! 3, red), (title' !! 6, blue)]
  wait 2
  -- move title back down
  waitOn $ mapM_ (fork . moveVertical (-2)) title'
  wait 2
  -- hide everything
  oFadeHideAll title'
  wait 1
