{-|
Module      : Common.SVG
Description : Common SVG related utilities.
Copyright   : (c) Xie Ruifeng, 2020
License     : AGPL-3
Maintainer  : krantz.xrf@outlook.com
Stability   : experimental
Portability : portable
-}
module Common.SVG where

import Control.Lens

import Graphics.SvgTree.Types

import Linear.V2     (V2 (V2))
import Linear.Vector ((*^))

-- |Flip the directon of an SVG path.
flipPath :: [PathCommand] -> [PathCommand]
flipPath ~(MoveTo OriginAbsolute (pInit : pRest) : cRest) = res where
  res = reverse (go Nothing pInit Nothing (LineTo OriginAbsolute pRest : cRest))
  step OriginAbsolute _ p' = p'
  step OriginRelative p p' = p + p'
  upd Nothing     p = Just p
  upd p0@(Just _) _ = p0
  go :: Maybe RPoint -> RPoint -> Maybe RPoint -> [PathCommand] -> [PathCommand]
  go _  p _ (MoveTo orig (p' : ps) : cmds)
    = MoveTo OriginAbsolute [p] : go Nothing (step orig p p') Nothing (LineTo orig ps : cmds)
  go p0 p _ (LineTo orig (p' : ps) : cmds)
    = LineTo OriginAbsolute [p] : go (upd p0 p) (step orig p p') Nothing (LineTo orig ps : cmds)
  go p0 p _ (HorizontalTo orig (x : xs) : cmds)
    = HorizontalTo OriginAbsolute [view _1 p]
    : go (upd p0 p) (step orig p (V2 x (view _2 p))) Nothing (HorizontalTo orig xs : cmds)
  go p0 p _ (VerticalTo orig (y : ys) : cmds)
    = VerticalTo OriginAbsolute [view _2 p]
    : go (upd p0 p) (step orig p (V2 (view _1 p) y)) Nothing (VerticalTo orig ys : cmds)
  go p0 p _ (CurveTo orig ((c1, c2, p') : ps) : cmds)
    = CurveTo OriginAbsolute [(step orig p c2, step orig p c1, p)]
    : go (upd p0 p) (step orig p p') (Just (step orig p c2)) (CurveTo orig ps : cmds)
  go p0 p c (SmoothCurveTo orig ((c2, p') : ps) : cmds) = case c of
    Nothing -> go p0 p c (QuadraticBezier orig [(c2, p')] : SmoothCurveTo orig ps : cmds)
    Just c1 -> go p0 p c (CurveTo OriginAbsolute
      [(2 *^ p - step orig p c1, step orig p c2, step orig p p')]
      : SmoothCurveTo orig ps : cmds)
  go p0 p _ (QuadraticBezier orig ((c, p') : ps) : cmds)
    = QuadraticBezier OriginAbsolute [(step orig p c, p)]
    : go (upd p0 p) (step orig p p') (Just (step orig p c)) (QuadraticBezier orig ps : cmds)
  go p0 p c (SmoothQuadraticBezierCurveTo orig (p' : ps) : cmds) = case c of
    Nothing -> go p0 p c (LineTo orig [p'] : SmoothQuadraticBezierCurveTo orig ps : cmds)
    Just c1 -> go p0 p c (QuadraticBezier OriginAbsolute [(2 *^ p - step orig p c1, step orig p p')]
      : SmoothQuadraticBezierCurveTo orig ps : cmds)
  go p0 p _ (EllipticalArc orig ((cx, cy, rot, fLarge, fSweep, p') : ps) : cmds)
    = let V2 cx' cy' = step orig p (V2 cx cy)
    in EllipticalArc OriginAbsolute [(cx', cy', rot, fLarge, not fSweep, p)]
    : go p0 (step orig p p') Nothing (EllipticalArc orig ps : cmds)
  go Nothing   p c (EndPath : cmds) = go Nothing p c cmds
  go (Just p0) p c (EndPath : cmds) = go (Just p0) p c (LineTo OriginAbsolute [p0] : cmds)
  go p0 p c (_ : cmds) = go p0 p c cmds
  go _  p _ [] = [MoveTo OriginAbsolute [p]]
