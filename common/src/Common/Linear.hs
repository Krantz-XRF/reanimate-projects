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

-- |Types which allows for linear interpolation.
class Linear a where
  -- |Linear interpolation. 0 for departure, 1 for destination.
  lerp :: Double -> a -> a -> a

instance Linear Pixel8 where
  lerp t x y = round (lerp @Double t (fromIntegral x) (fromIntegral y))

instance Linear Double where
  lerp t x y = (1 - t) * x + t * y

instance Linear PixelRGBA8 where
  lerp t (PixelRGBA8 r1 g1 b1 a1) (PixelRGBA8 r2 g2 b2 a2)
    = PixelRGBA8 (lerp t r1 r2) (lerp t g1 g2) (lerp t b1 b2) (lerp t a1 a2)
