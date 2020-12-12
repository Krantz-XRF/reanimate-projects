{-|
Module      : Common.Linear
Description : Linear interpolations.
Copyright   : (c) Xie Ruifeng, 2020
License     : AGPL-3
Maintainer  : krantz.xrf@outlook.com
Stability   : experimental
Portability : portable
-}
{-# LANGUAGE TypeFamilies #-}
module Common.Linear where

import Codec.Picture.Types
import Reanimate.ColorComponents

import qualified Linear as L

-- |Types which allows for linear interpolation.
class Linear a where
  -- |Linear interpolation. 0 for departure, 1 for destination.
  lerp :: Double -> a -> a -> a

instance Linear Pixel8 where
  lerp t x y = round (lerp @Double t (fromIntegral x) (fromIntegral y))

instance Linear Double where
  lerp t x y = (1 - t) * x + t * y

instance Linear PixelRGBA8 where
  lerp t x y = interpolateRGBA8 labComponents x y t

instance L.Additive f => Linear (f Double) where
  lerp = L.lerp . (1 -)
