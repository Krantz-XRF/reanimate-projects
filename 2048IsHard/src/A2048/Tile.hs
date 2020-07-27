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

import Text.Printf
import qualified Data.Text as T

import Control.Lens
import Control.Monad.Reader.Class

import Reanimate
import Graphics.SvgTree

import A2048.Config
import A2048.Text

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
rawTile bg a r = roundedSquare a r & fillColor .~ pure bg

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
  a <- asks (view tileSize)
  r <- asks (view tileRadius)
  showLabel <- asks (view tileShowLabel)
  let rect = rawTile bg a r
  if not showLabel then pure rect else
    tileLabel l >>= \lbl -> pure (mkGroup [rect, lbl])

-- |Tile number. Read configuration to determine size and colour.
tileLabel :: (HasGame2048Config c, MonadReader c m) => Int -> m SVG
tileLabel l = do
  fg <- asks (tileTextColourOf l)
  a <- asks (view tileSize)
  s <- asks (view tileTextScaleRatio)
  usingLog <- asks (view useLogarithm)
  let lbl = if usingLog then show l else show (2 ^ l :: Int)
  let sf = printf "\\textsf{%s}" lbl
  let tex = if s >= 0.6 then printf "\\textbf{%s}" sf else sf
  let txt = scale s $ colourLabel fg $ T.pack tex
  let ratio = a * 0.8 / svgWidth txt
  let txt' = if ratio < 1 then scale ratio txt else txt
  pure (center txt')
