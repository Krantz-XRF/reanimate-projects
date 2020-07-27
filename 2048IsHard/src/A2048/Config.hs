{-|
Module      : A2048.Config
Description : Rendering configuration.
Copyright   : (c) Xie Ruifeng, 2020
License     : AGPL-3
Maintainer  : krantz.xrf@outlook.com
Stability   : experimental
Portability : portable
-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE Strict #-}
module A2048.Config where

import Control.Lens
import Graphics.SvgTree
import A2048.HexColour

-- |Configurations for the 2048 Game.
data Game2048Config = Game2048Config
  { _tileSize :: Double
  , _tileRadius :: Double
  , _tileTextScaleRatio :: Double
  , _tileFillColour :: [Texture]
  , _tileTextColour :: [Texture]
  , _tileShowLabel :: Bool
  , _useLogarithm :: Bool
  , _boardWidth :: Int
  , _boardHeight :: Int
  , _boardGapSize :: Double
  , _boardBorderSize :: Double
  , _boardGridColour :: Texture
  , _boardFillColour :: Texture
  , _motionFillPadding :: Bool
  , _motionDuration :: Double
  } deriving stock (Show)

-- |Classy lens for 'Game2048Config'.
makeClassy ''Game2048Config

-- |Default configuration for tiles.
defaultGame2048Config :: Game2048Config
defaultGame2048Config = Game2048Config
  { _tileSize = 1.2
  , _tileRadius = 0.09
  , _tileTextScaleRatio = 0.8
  , _tileFillColour = tileBgColours
  , _tileTextColour = tileFgColours
  , _tileShowLabel = True
  , _useLogarithm = False
  , _boardWidth = 4
  , _boardHeight = 4
  , _boardGapSize = 0.2
  , _boardBorderSize = 0.3
  , _boardGridColour = [rgba|cdc0b4|]
  , _boardFillColour = [rgba|bbada0|]
  , _motionFillPadding = False
  , _motionDuration = 0.4
  }

-- |Scale the whole game UI.
scaleGame :: Double -> Game2048Config -> Game2048Config
scaleGame r cfg@Game2048Config{..} = cfg
  { _tileSize           = r * _tileSize
  , _tileRadius         = r * _tileRadius
  , _tileTextScaleRatio = r * _tileTextScaleRatio
  , _boardGapSize       = r * _boardGapSize
  , _boardBorderSize    = r * _boardBorderSize
  }

-- |Tile background colours.
tileBgColours :: [Texture]
tileBgColours =
  [ [rgba|eee4da|], [rgba|ede0c8|], [rgba|f2b179|], [rgba|f59563|]
  , [rgba|f67c5f|], [rgba|f65e3b|], [rgba|edcf72|], [rgba|edcc61|]
  , [rgba|edc850|], [rgba|edc53f|], [rgba|edc22e|], [rgba|3c3a32|]
  ]

-- |Tile foreground colours.
tileFgColours :: [Texture]
tileFgColours =
  [ [rgba|776e65|], [rgba|776e65|], [rgba|f9f6f2|], [rgba|f9f6f2|]
  , [rgba|f9f6f2|], [rgba|f9f6f2|], [rgba|f9f6f2|], [rgba|f9f6f2|]
  , [rgba|f9f6f2|], [rgba|f9f6f2|], [rgba|f9f6f2|], [rgba|f9f6f2|]
  ]

-- |Short-cut for tile background colour.
tileFillColourOf :: HasGame2048Config c => Int -> c -> Texture
tileFillColourOf l cfg = view tileFillColour cfg !! (l - 1)

-- |Short-cut for tile foreground colour.
tileTextColourOf :: HasGame2048Config c => Int -> c -> Texture
tileTextColourOf l cfg = view tileTextColour cfg !! (l - 1)
