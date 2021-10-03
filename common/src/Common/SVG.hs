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

import Control.Arrow
import Control.Lens
import Data.List

import Graphics.SvgTree.Types

import Linear.Metric (distance)
import Linear.V2     (V2 (V2))
import Linear.Vector ((*^))

import Reanimate.Animation
import Reanimate.Svg

-- |Adjust the @strokeWidth@ for an 'SVG'.
adjustStrokeWidth :: (Double -> Double) -> SVG -> SVG
adjustStrokeWidth f = mapTree (strokeWidth . _Just %~ mapNumber f)

-- |Predefined style for paths: every path is counterclockwise, and left paths appear first.
counterclockwiseLR :: [PathCommand] -> [PathCommand]
counterclockwiseLR = concatMap makeCounterclockwise . sortOn (view _1 . pathBB) . groupPath

-- |Predefined style for paths: every path is clockwise, and left paths appear first.
clockwiseLR :: [PathCommand] -> [PathCommand]
clockwiseLR = concatMap makeClockwise . sortOn (view _1 . pathBB) . groupPath

-- |Group a series of 'PathCommand's by subpaths.
groupPath :: [PathCommand] -> [[PathCommand]]
groupPath = groupBy grouping
  where grouping _ (MoveTo _ _) = False
        grouping _ _            = True

-- |Sort subpaths by some ordering.
sortPathBy :: ([PathCommand] -> [PathCommand] -> Ordering) -> [PathCommand] -> [PathCommand]
sortPathBy cmp = concat . sortBy cmp . groupPath

-- |Sort subpaths by comparing the results of a key function applied to each element.
--
-- @sortPathOn f@ is equivalent to @sortPathBy (comparing f)@, but has the performance
-- advantage of only evaluating @f@ once for each element in the input list. This is
-- called the decorate-sort-undecorate paradigm, or Schwartzian transform.
--
-- See also 'sortOn' and 'sortBy'.
--
-- For predefined key functions, see 'pathBB', 'pathCenter', 'pathRadius'.
sortPathOn :: Ord a => ([PathCommand] -> a) -> [PathCommand] -> [PathCommand]
sortPathOn f = concat . sortOn f . groupPath

-- |Bounding box of a (sub-) path.
pathBB :: [PathCommand] -> (Coord, Coord, Coord, Coord)
pathBB p = boundingBox (PathTree (defaultSvg & pathDefinition .~ p))

-- |Center of a path.
pathCenter :: [PathCommand] -> RPoint
pathCenter pth = V2 (x0 + w / 2) (y0 + h / 2)
  where (!x0, !y0, !w, !h) = pathBB pth

-- |Radius of a (sub-) path, i.e. maximum distance from the center of the bounding box.
pathRadius :: [PathCommand] -> Coord
pathRadius pth = maximum (map (distance c) points)
  where c = pathCenter pth
        points = getPoints (toLineCommands pth)

-- |Get end points and control points for a path (as @[LineCommand]@).
getPoints :: [LineCommand] -> [RPoint]
getPoints = go []
  where go res []                     = res
        go res (LineMove p0 : rest)   = go (p0 : res) rest
        go res (LineBezier ps : rest) = go (ps ++ res) rest
        go res (LineEnd p : rest)     = go (p : res) rest

-- |Flip the directon of an SVG path (as @[LineCommand]@).
-- Prefer 'flipPath' over roundtripping through 'toLineCommands' and 'lineToPath'.
flipLine :: [LineCommand] -> [LineCommand]
flipLine = reverse . uncurry (zipWith flipSegment) . (id &&& (undefined :) . map extractPos)
  where flipSegment (LineMove p0)   _ = LineEnd p0
        flipSegment (LineBezier ps) c = LineBezier (c : reverse (init ps))
        flipSegment (LineEnd p)     _ = LineMove p
        extractPos (LineMove p0)   = p0
        extractPos (LineBezier ps) = last ps
        extractPos (LineEnd p)     = p

-- |Check whether a (closed) path is clockwise or counterclockwise.
isClockwise :: [RPoint] -> Bool
isClockwise ps = sum (zipWith area2 (tail ps ++ [head ps]) ps) >= 0
  where area2 (V2 x0 y0) (V2 x y) = (x - x0) * (y + y0)
  -- area2 calculates 2x the area under the current segment

-- |Make a path clockwise by flipping if it was not.
makeClockwise :: [PathCommand] -> [PathCommand]
makeClockwise ps = if isClockwise (getPoints (toLineCommands ps)) then ps else flipPath ps

-- |Make a path clockwise by flipping if it was not.
makeCounterclockwise :: [PathCommand] -> [PathCommand]
makeCounterclockwise ps = if isClockwise (getPoints (toLineCommands ps)) then flipPath ps else ps

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
