module A2048.Tile where

import Data.Monoid
import qualified Data.Text as T

import Control.Lens
import Control.Monad.Reader

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

tile :: (HasConfig c, MonadReader c m) => Int -> m SVG
tile l = do
  bg <- asks (tileFillColourOf l)
  fg <- asks (tileTextColourOf l)
  a <- asks (view tileSize)
  r <- asks (view tileRadius)
  let rect = roundedSquare a r & fillColor .~ Last (Just bg)
  usingLog <- asks (view useLogarithm)
  let label = T.pack $ if usingLog then show l else show (2 ^ l :: Int)
  let txt = latex label & fillColor .~ Last (Just fg)
  pure (mkGroup [rect, center txt])
