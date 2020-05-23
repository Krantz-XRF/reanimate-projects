{-# LANGUAGE TemplateHaskell #-}
module A2048.HexColour where

import Data.Char
import Data.Word

import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote

import Graphics.SvgTree.Types
import Codec.Picture.Types

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
  in [| ColorRef (PixelRGBA8 r g b a) |]
makeRgba [r1, r2, g1, g2, b1, b2] =
  let r = makeHex r1 r2
      g = makeHex g1 g2
      b = makeHex b1 b2
      makeHex x y = fromIntegral (digitToInt x * 16 + digitToInt y) :: Word8
  in [| ColorRef (PixelRGBA8 r g b 1) |]
makeRgba [r, g, b, a] = makeRgba [r, r, g, g, b, b, a, a]
makeRgba [r, g, b] = makeRgba [r, r, g, g, b, b]
makeRgba _ = fail "Invalid RGBA colour."
