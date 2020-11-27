{-|
Module      : A3SAT.LaTeX
Description : Print boolean expressions as LaTeX sequence.
Copyright   : (c) Xie Ruifeng, 2020
License     : AGPL-3
Maintainer  : krantz.xrf@outlook.com
Stability   : experimental
Portability : portable
-}
{-# LANGUAGE OverloadedStrings #-}
module A3SAT.LaTeX where

import qualified Data.Text    as T
import qualified Data.Text.IO as T

import Data.Foldable
import Data.List

import Control.Monad.State

import Graphics.SvgTree
import Reanimate

import A3SAT.Expression

-- |Get human-readable text for a boolean expression.
prettyShow :: [T.Text] -> LExpression a -> T.Text
prettyShow vars = fold . go where
  go (LVar _ x)     = LVar (vars `genericIndex` x) x
  go (LNot _ a)     = LNot "!" (go a)
  go (LAnd a _ b)   = LAnd (go a) " & " (go b)
  go (LOr a _ b)    = LOr (go a) " | " (go b)
  go (LParen _ a _) = LParen "(" (go a) ")"

-- |Print human-readable text for a boolean expression to stdout.
prettyPrint :: [T.Text] -> LExpression a -> IO ()
prettyPrint vars = T.putStrLn . prettyShow vars

-- |Get LaTeX sequence for a boolean expression.
showLaTeX :: [T.Text] -> LExpression a -> T.Text
showLaTeX vars = fold . go where
  go (LVar _ x)     = LVar (vars `genericIndex` x) x
  go (LNot _ a)     = LNot "\\neg " (go a)
  go (LAnd a _ b)   = LAnd (go a) " \\land " (go b)
  go (LOr a _ b)    = LOr (go a) " \\lor " (go b)
  go (LParen _ a _) = LParen "(" (go a) ")"

-- |Given number of glyphs for variables, label the position of each element.
-- This function runs in a 'State' monad, keeping track of the total count of glyphs.
locateGlyphsM :: MonadState Int m => [Int] -> LExpression a -> m (LExpression (a, Int))
locateGlyphsM g = mapExprM handleOp where
  handleOp e y = gets (y, ) <* modify (+ glyphCount g e)

-- |Given number of glyphs for variables, label the position of each element.
locateGlyphs :: [Int] -> LExpression a -> LExpression (a, Int)
locateGlyphs g e = evalState (locateGlyphsM g e) 0

-- |Given number of glyphs for variables, associate each element with its glyphs.
-- This function runs in a 'State' monad, keeping track of the remaining glyphs.
associateGlyphsM :: MonadState [SVG] m => [Int] -> LExpression a -> m (LExpression (a, SVG))
associateGlyphsM g = mapExprM handleOp where
  handleOp (glyphCount g -> c) y = gets ((y, ) . takeGlyph c) <* modify (drop c)
  takeGlyph 0 = const None
  takeGlyph 1 = head
  takeGlyph n = mkGroup . take n

-- |Given number of glyphs for variables, associate each element with its glyphs.
associateGlyphs :: [SVG] -> [Int] -> LExpression a -> LExpression (a, SVG)
associateGlyphs g c e = evalState (associateGlyphsM c e) g
