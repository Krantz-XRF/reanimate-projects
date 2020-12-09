{-|
Module      : Common.HexColour
Description : Hexadecimal colour representation.
Copyright   : (c) Xie Ruifeng, 2020
License     : AGPL-3
Maintainer  : krantz.xrf@outlook.com
Stability   : experimental
Portability : portable
-}
{-# LANGUAGE TemplateHaskell #-}
module Common.HexColour (rgba, FromRGBA8(..)) where

import Data.Char
import Data.Word

import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax

import Codec.Picture.Types
import Graphics.SvgTree.Types

import Skylighting.Types (Color (RGB))

-- |All the things that can be converted from a 'PixelRGBA8'.
class FromRGBA8 a where
  -- |Convert from 'PixelRGBA8'.
  fromRGBA8 :: PixelRGBA8 -> a

instance FromRGBA8 PixelRGBA8 where
  fromRGBA8 = id

instance FromRGBA8 Texture where
  fromRGBA8 = ColorRef

instance FromRGBA8 Color where
  fromRGBA8 (PixelRGBA8 r g b _) = RGB r g b

instance FromRGBA8 a => FromRGBA8 (Maybe a) where
  fromRGBA8 = Just . fromRGBA8

-- |The 'rgba' Quasi-Quoter.
-- Accepted format: RGB, RGBA, RRGGBB, RRGGBBAA
rgba :: QuasiQuoter
rgba = QuasiQuoter
  { quoteExp = makeRgba
  , quotePat = undefined
  , quoteType = undefined
  , quoteDec = undefined
  }

makeRgba :: String -> Q Exp
makeRgba [r1, r2, g1, g2, b1, b2, a1, a2] =
  let r = makeHex r1 r2
      g = makeHex g1 g2
      b = makeHex b1 b2
      a = makeHex a1 a2
      makeHex x y = fromIntegral (digitToInt x * 16 + digitToInt y) :: Word8
  in [| fromRGBA8 (PixelRGBA8 r g b a) |]
makeRgba [r1, r2, g1, g2, b1, b2] = makeRgba [r1, r2, g1, g2, b1, b2, 'F', 'F']
makeRgba [r, g, b, a] = makeRgba [r, r, g, g, b, b, a, a]
makeRgba [r, g, b] = makeRgba [r, r, g, g, b, b]
makeRgba _ = fail "Invalid RGBA colour."
