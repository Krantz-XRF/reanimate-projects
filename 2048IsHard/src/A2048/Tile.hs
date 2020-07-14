{-|
Module      : A2048.Tile
Description : Tiles for the 2048 game.
Copyright   : (c) Xie Ruifeng, 2020
License     : AGPL-3
Maintainer  : krantz.xrf@outlook.com
Stability   : experimental
Portability : portable
-}
module A2048.Tile where

import Data.Monoid
import Text.Printf
import qualified Data.Text as T

import Control.Lens
import Control.Monad.Reader.Class

import Reanimate
import Graphics.SvgTree

import A2048.Config

-- |Make a rounded rectangle.
roundedRect :: Double -> Double -> Double -> SVG
roundedRect w h r = RectangleTree
  $ defaultSvg
  & rectWidth ?~ Num w
  & rectHeight ?~ Num h
  & rectUpperLeftCorner .~ (Num $ -w / 2, Num $ -h / 2)
  & rectCornerRadius .~ (Just $ Num r, Just $ Num r)

-- |Make a rounded square.
roundedSquare :: Double -> Double -> SVG
roundedSquare a = roundedRect a a

-- |Raw tile: coloured rounded rectangle.
rawTile :: Texture -> Double -> Double -> SVG
rawTile bg a r = roundedSquare a r & fillColor .~ Last (Just bg)

-- |Empty tile. Read configuration to determine size and colour.
emptyTile :: (HasGame2048Config c, MonadReader c m) => m SVG
emptyTile = rawTile
  <$> asks (view boardGridColour)
  <*> asks (view tileSize)
  <*> asks (view tileRadius)

-- |Tile with number. Read configuration to determine size and colour.
tile :: (HasGame2048Config c, MonadReader c m) => Int -> m SVG
tile 0 = pure None
tile l = do
  bg <- asks (tileFillColourOf l)
  fg <- asks (tileTextColourOf l)
  a <- asks (view tileSize)
  r <- asks (view tileRadius)
  let rect = rawTile bg a r
  usingLog <- asks (view useLogarithm)
  let label = if usingLog then show l else show (2 ^ l :: Int)
  let tex = if a >= 0.8 then printf "\\textbf{%s}" label else label
  let txt = latex (T.pack tex) & fillColor .~ Last (Just fg)
  let ratio = a * 0.85 / svgWidth txt
  let txt' = if ratio < 1 then scale ratio txt else txt
  pure (mkGroup [rect, center txt'])
