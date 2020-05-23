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
module A2048.Config where

import Control.Lens
import Graphics.SvgTree
import A2048.HexColour

-- |Configurations related to tiles.
data TileConfig = TileConfig
  { _tileSize :: Double
  , _tileRadius :: Double
  , _tileFillColour :: [Texture]
  , _tileTextColour :: [Texture]
  , _useLogarithm :: Bool
  } deriving stock (Show)

-- |Classy lens for 'TileConfig'.
makeClassy ''TileConfig

-- |Default configuration for tiles.
defaultTileConfig :: TileConfig
defaultTileConfig = TileConfig
  { _tileSize = 1.2
  , _tileRadius = 0.09
  , _tileFillColour = tileBgColours
  , _tileTextColour = tileFgColours
  , _useLogarithm = False
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
tileFillColourOf :: HasTileConfig c => Int -> c -> Texture
tileFillColourOf l cfg = view tileFillColour cfg !! (l - 1)

-- |Short-cut for tile foreground colour.
tileTextColourOf :: HasTileConfig c => Int -> c -> Texture
tileTextColourOf l cfg = view tileTextColour cfg !! (l - 1)

-- |Configurations related to the board.
data BoardConfig = BoardConfig
  { _boardTileConfig :: TileConfig
  , _boardWidth :: Int
  , _boardHeight :: Int
  , _boardGapSize :: Double
  , _boardBorderSize :: Double
  , _boardGridColour :: Texture
  , _boardFillColour :: Texture
  } deriving stock (Show)

-- |Classy lens for 'BoardConfig'.
makeClassy ''BoardConfig

-- |Default configuration for the board.
defaultBoardConfig :: BoardConfig
defaultBoardConfig = BoardConfig
  { _boardTileConfig = defaultTileConfig
  , _boardWidth = 4
  , _boardHeight = 4
  , _boardGapSize = 0.2
  , _boardBorderSize = 0.3
  , _boardGridColour = [rgba|cdc0b4|]
  , _boardFillColour = [rgba|bbada0|]
  }

instance HasTileConfig BoardConfig where
  tileConfig = boardTileConfig
