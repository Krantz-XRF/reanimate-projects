{-|
Module      : A3SAT.Animation.Primitives
Description : Basic animations for 3-SAT problem.
Copyright   : (c) Xie Ruifeng, 2020
License     : AGPL-3-or-later
Maintainer  : krantz.xrf@outlook.com
Stability   : experimental
Portability : portable
-}
{-# LANGUAGE OverloadedStrings #-}
module A3SAT.Animation.Primitives where

import qualified Data.Text as T

import Control.Applicative
import Control.Lens
import Data.Coerce
import Data.Foldable
import Data.List

import A3SAT.Expression
import A3SAT.Expression.Lens
import A3SAT.LaTeX

import Graphics.SvgTree
import Reanimate
import Reanimate.LaTeX
import Reanimate.Morph.Common
import Reanimate.Morph.Linear

import Common.Animation.Effects
import Common.Bifunctor

-- |Render the boolean expression with LaTeX, then associate nodes with glyphs.
showSvg :: [T.Text] -> LExpression l -> LExpression (l, SVG)
showSvg xs e = associateGlyphs glyphs (map countGlyphs xs) e
  where countGlyphs = length . svgGlyphs . mkTex
        glyphs = [ f a | (f, _, a) <- svgGlyphs $ mkTex $ showLaTeX xs e ]
        mkTex = center . latexWithHeaders ["\\usepackage{bm}"] . mkMath
        mkMath x = "$" <> x <> "$"

-- |Render the boolean expression with LaTeX, drop the original labels, then
-- associate nodes with glyphs.
showSvg' :: [T.Text] -> LExpression l -> LExpression SVG
showSvg' xs = fmap snd . showSvg xs

-- |Modify the variables.
--
-- @'mapVar' idx attrs filt upd@ does the following:
--
-- * Locate all the variables using 'vars', filtered by @filt@
-- * Focus on the targeted parts with the given 'Lens'' @idx@
-- * Modify that part with @upd@, provided the corresponding attribute
--   in @attrs@, indexed by the 'Word' index of that variable
mapVar :: ASetter' l b -> [a] -> (Word -> Bool) -> (a -> b -> b) -> LExpression l -> LExpression l
mapVar idx attrs filt upd = over (vars filt) updVar
  where updVar (l, n) = (over idx (upd (attrs `genericIndex` n)) l, n)

-- |Colour the variables.
colourVar :: Lens' l SVG -> [Texture] -> LExpression l -> LExpression l
colourVar idx cols = mapVar idx cols (const True) upd
  where upd c = fillColor .~ pure c

-- |Colour labels by category.
colourLabel :: ASetter' (LExpression l) SVG -> Texture -> LExpression l -> LExpression l
colourLabel idx col = over idx (fillColor .~ pure col)

-- |Collect all SVGs in an expression.
exprSvg :: LExpression SVG -> SVG
exprSvg = mkGroup . toList

-- |Perform linear morphology on already-paired-up images.
morphAnim :: LExpression (SVG, SVG) -> Animation
morphAnim = parallel . fmap (animate . uncurry (morph linear))

-- |Perform linear morphology on a pair of expressions.
morphAnimZip :: LExpression SVG -> LExpression SVG -> Animation
morphAnimZip a b = parallel $ zipExprWith svgMorph exprMorph a b
  where svgMorph x y = animate (morph linear x y)
        exprMorph x y = LVar (animate (morph linear (exprSvg x) (exprSvg y))) undefined

-- |Evaluate a boolean expression.
evalExpr :: [Bool] -> LExpression l -> Bool
evalExpr bs = runIdentity . evalExprF (coerce bs)

-- |Evaluate a boolean expression, with its label including boolean values.
evalExprIdx :: Applicative f => Lens' l (f Bool) -> LExpression l -> LExpression l
evalExprIdx idx = scanFixIdx idx (evalExprAlg const)

-- |Evaluate a boolean expression in an 'Applicative' environment.
-- Provided a list of boolean values.
evalExprF :: Applicative f => [f Bool] -> LExpression l -> f Bool
evalExprF bs = evalExprWith (\_ -> genericIndex bs)

-- |Evaluate a boolean expression in an 'Applicative' environment.
-- Provided a function from labels and indices to boolean values.
evalExprWith :: Applicative f => (l -> Word -> f Bool) -> LExpression l -> f Bool
evalExprWith f = foldFix (evalExprAlg f)

-- |Algebra @ExpressionF (f Bool) l -> f Bool@ for evaluating a boolean expression.
evalExprAlg :: Applicative f => (l -> Word -> f Bool) -> ExpressionF (f Bool) l -> f Bool
evalExprAlg f (VarF l n)     = f l n
evalExprAlg _ (NotF _ a)     = fmap not a
evalExprAlg _ (AndF a _ b)   = liftA2 (&&) a b
evalExprAlg _ (OrF a _ b)    = liftA2 (||) a b
evalExprAlg _ (ParenF _ a _) = a

-- |Bounding box for expressions.
exprBoundingBox :: LExpression SVG -> (Double, Double, Double, Double)
exprBoundingBox = foldl1' mergeBox . toList . fmap boundingBox
  where mergeBox (x1, y1, w1, h1) (x2, y2, w2, h2) =
          let x = min x1 x2
              y = min y1 y2
              w = max (x1 + w1) (x2 + w2) - x
              h = max (y1 + h1) (y2 + h2) - y
          in (x, y, w, h)

-- |Make a bounding rectangle for the expression.
mkBoundingRect :: LExpression SVG -> SVG
mkBoundingRect = pathify . convRect . exprBoundingBox
  where convRect (x, y, w, h) = translate (x + w/2) (y + h/2) (mkRect w h)
