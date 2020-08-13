{-|
Module      : A3SAT.Animation.Primitives
Description : Basic animations for 3-SAT problem.
Copyright   : (c) Xie Ruifeng, 2020
License     : AGPL-3
Maintainer  : krantz.xrf@outlook.com
Stability   : experimental
Portability : portable
-}
{-# LANGUAGE OverloadedStrings #-}
module A3SAT.Animation.Primitives where

import qualified Data.Text as T

import Control.Lens
import Data.Foldable
import Data.List

import A3SAT.Expression
import A3SAT.Expression.Lens
import A3SAT.LaTeX

import Graphics.SvgTree
import Reanimate
import Reanimate.Morph.Common
import Reanimate.Morph.Linear

-- |Render the boolean expression with LaTeX, then associate nodes with glyphs.
showSvg :: [T.Text] -> LExpression l -> LExpression (l, SVG)
showSvg xs e = associateGlyphs glyphs (map countGlyphs xs) e
  where countGlyphs = length . svgGlyphs . mkTex
        glyphs = [ f a | (f, _, a) <- svgGlyphs $ mkTex $ showLaTeX xs e ]
        mkTex = withFillColor "black" . center . latex . mkMath
        mkMath x = "$" <> x <> "$"

-- |Modify the variables.
--
-- @'mapVar' idx attrs filt upd@ does the following:
--
-- * Locate all the variables using 'vars', filtered by @filt@
-- * Focus on the targeted parts with the given 'Lens'' @idx@
-- * Modify that part with @upd@, provided the corresponding attribute
--   in @attrs@, indexed by the 'Word' index of that variable
mapVar :: Lens' l b -> [a] -> (Word -> Bool) -> (a -> b -> b) -> LExpression l -> LExpression l
mapVar idx attrs filt upd = over (vars filt) updVar
  where updVar (l, n) = (over idx (upd (attrs `genericIndex` n)) l, n)

-- |Colour the variables.
colourVar :: Lens' l SVG -> [Texture] -> LExpression l -> LExpression l
colourVar idx cols = mapVar idx cols (const True) upd
  where upd c = fillColor .~ pure c

-- |Colour labels by category.
colourLabel :: Lens' (LExpression l) SVG -> Texture -> LExpression l -> LExpression l
colourLabel idx col = over idx (fillColor .~ pure col)

-- |Collect all SVGs in an expression.
exprSvg :: LExpression SVG -> SVG
exprSvg = mkGroup . toList

-- |Perform linear morphology on already-paired-up images.
morphAnim :: LExpression (SVG, SVG) -> Animation
morphAnim = foldl' parA (pause 0) . toList . fmap (animate . uncurry (morph linear))

-- |Perform linear morphology on a pair of expressions.
morphAnimZip :: LExpression SVG -> LExpression SVG -> Animation
morphAnimZip a b = foldl' parA (pause 0) $ toList $ zipExprWith svgMorph exprMorph a b
  where svgMorph x y = animate (morph linear x y)
        exprMorph x y = LVar (animate (morph linear (exprSvg x) (exprSvg y))) undefined
