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

import           Data.Functor
import qualified Data.Text    as T
import           Text.Printf

import Control.Lens
import Control.Monad.Reader.Class

import Graphics.SvgTree
import Reanimate

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
  <$> view boardGridColour
  <*> view tileSize
  <*> view tileRadius

-- |Tile with number. Read configuration to determine size and colour.
tile :: (HasGame2048Config c, MonadReader c m) => Int -> m SVG
tile 0 = pure None
tile l = do
  bg <- asks (tileFillColourOf l)
  a <- view tileSize
  r <- view tileRadius
  showLabel <- view tileShowLabel
  let rect = rawTile bg a r
  if not showLabel then pure rect else
    tileLabel l <&> \lbl -> mkGroup [rect, lbl]

-- |Tile number. Read configuration to determine size and colour.
tileLabel :: (HasGame2048Config c, MonadReader c m) => Int -> m SVG
tileLabel l = do
  fg <- asks (tileTextColourOf l)
  a <- view tileSize
  s <- view tileTextScaleRatio
  lbl <- view tileLabelMode <&> \case
    Logarithm -> printf "\\textsf{%d}" l
    Plain -> printf "\\textsf{%d}" (2 ^ l :: Int)
    Exponent -> printf "$\\textsf{2}^\\textsf{%d}$" l
  let tex = if s >= 0.6 then printf "\\textbf{%s}" lbl else lbl
  let txt = scale s $ colourLabel fg $ T.pack tex
  let ratio = a * 0.8 / svgWidth txt
  let txt' = if ratio < 1 then scale ratio txt else txt
  pure (center txt')
