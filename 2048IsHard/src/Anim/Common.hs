{-|
Module      : Anim.Common
Description : Common Reanimate pattern functions.
Copyright   : (c) Xie Ruifeng, 2020
License     : AGPL-3
Maintainer  : krantz.xrf@outlook.com
Stability   : experimental
Portability : portable
-}
module Anim.Common where

import Reanimate

-- |Add a white background for the given animation.
addWhiteBkg :: SVG -> SVG
addWhiteBkg svg = mkGroup [mkBackground "white", svg]

-- |Fade out the animation after it stops.
fadeToEnd :: Time -> Animation -> Animation
fadeToEnd d a =
  let lastFrame = frameAt (duration a) a
      animFade = applyE fadeOutE (staticFrame 1 lastFrame)
  in a `seqA` setDuration d animFade
