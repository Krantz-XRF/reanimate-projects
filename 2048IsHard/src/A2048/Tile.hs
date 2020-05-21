module A2048.Tile where

import Data.Monoid
import qualified Data.Text as T

import Control.Lens
import Control.Monad.Reader.Class

import Reanimate
import Graphics.SvgTree

import A2048.Config

roundedRect :: Double -> Double -> Double -> SVG
roundedRect w h r = RectangleTree
  $ defaultSvg
  & rectWidth ?~ Num w
  & rectHeight ?~ Num h
  & rectUpperLeftCorner .~ (Num $ -w / 2, Num $ -h / 2)
  & rectCornerRadius .~ (Just $ Num r, Just $ Num r)

roundedSquare :: Double -> Double -> SVG
roundedSquare a = roundedRect a a

rawTile :: Texture -> Double -> Double -> SVG
rawTile bg a r = roundedSquare a r & fillColor .~ Last (Just bg)

emptyTile :: (HasTileConfig c, HasBoardConfig c, MonadReader c m) => m SVG
emptyTile = rawTile
  <$> asks (view boardGridColour)
  <*> asks (view tileSize)
  <*> asks (view tileRadius)

tile :: (HasTileConfig c, MonadReader c m) => Int -> m SVG
tile l = do
  bg <- asks (tileFillColourOf l)
  fg <- asks (tileTextColourOf l)
  a <- asks (view tileSize)
  r <- asks (view tileRadius)
  let rect = rawTile bg a r
  usingLog <- asks (view useLogarithm)
  let label = if usingLog then show l else show (2 ^ l :: Int)
  let txt = latex (T.pack label) & fillColor .~ Last (Just fg)
  let ratio = a * 0.85 / svgWidth txt
  let txt' = if ratio < 1 then scale ratio txt else txt
  pure (mkGroup [rect, center txt'])
