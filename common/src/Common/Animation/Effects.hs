{-|
Module      : Common.Animation.Effects
Description : Common Reanimate pattern functions.
Copyright   : (c) Xie Ruifeng, 2020
License     : AGPL-3
Maintainer  : krantz.xrf@outlook.com
Stability   : experimental
Portability : portable
-}
module Common.Animation.Effects where

import Graphics.SvgTree
import Reanimate
import Reanimate.ColorComponents

-- |Add a white background for the given animation.
addWhiteBkg :: SVG -> SVG
addWhiteBkg svg = mkGroup [mkBackground "white", svg]

-- |Fade out the animation after it stops.
fadeToEnd :: Time -> Animation -> Animation
fadeToEnd d a =
  let lastFrame = frameAt (duration a) a
      animFade = applyE fadeOutE (staticFrame 1 lastFrame)
  in a `seqA` setDuration d animFade

-- |Linear interpolation (for position) between 2 key frames.
lerpSVG :: SVG -> SVG -> Time -> SVG
lerpSVG a1 a2 t =
  translate (lerp t cx2 cx1) (lerp t cy2 cy1)
  $ scale (lerp t 1 (w1 / w2))
  $ center a2
  where (x1, y1, w1, h1) = boundingBox a1
        (x2, y2, w2, h2) = boundingBox a2
        (cx1, cy1) = (x1 + w1 / 2, y1 + h1 / 2)
        (cx2, cy2) = (x2 + w2 / 2, y2 + h2 / 2)
        lerp r x y = r * x + (1 - r) * y

-- |Linear interpolation between 2 colours.
lerpColour :: Texture -> Texture -> Time -> Texture
lerpColour ~(ColorRef x) ~(ColorRef y) t
  = ColorRef (interpolateRGBA8 labComponents x y t)

-- |Make a pure translation animation.
translationAnim :: SVG -> SVG -> Animation
translationAnim a1 a2 = animate (lerpSVG a1 a2)

-- |Wiggle = S-Curve, sin, S-Curve.
-- Copied from reanimate/examples/tut_glue_latex.hs
wiggleS :: Signal
wiggleS t
  | t < 0.25  = curveS 2 (t * 4)
  | t < 0.75  = sin ((t - 0.25) * 2 * pi + pi / 2)
  | otherwise = curveS 2 ((t - 0.75) * 4) - 1

-- |Highlight = Wiggle (Rotate) + Scale.
-- Copied from reanimate/examples/tut_glue_latex.hs
highlightE :: Double -> Effect
highlightE r d t
  = scale (1 + bellS 2 (t / d) * r)
  . rotate (wiggleS (t / d) * 20)
