{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module A2048.Config where

import Control.Lens
import Graphics.SvgTree
import A2048.HexColour

data Config = Config
  { _tileSize :: Double
  , _tileRadius :: Double
  , _tileGapSize :: Double
  , _tileFillColour :: [Texture]
  , _tileTextColour :: [Texture]
  , _useLogarithm :: Bool
  } deriving stock (Show)

makeClassy ''Config

defaultConfig :: Config
defaultConfig = Config
  { _tileSize = 1.2
  , _tileRadius = 0.07
  , _tileGapSize = 0.2
  , _tileFillColour = tileBgColours
  , _tileTextColour = tileFgColours
  , _useLogarithm = False
  }

tileBgColours :: [Texture]
tileBgColours =
  [ [rgba|eee4da|], [rgba|ede0c8|], [rgba|f2b179|], [rgba|f59563|]
  , [rgba|f67c5f|], [rgba|f65e3b|], [rgba|edcf72|], [rgba|edcc61|]
  , [rgba|edc850|], [rgba|edc53f|], [rgba|edc22e|], [rgba|3c3a32|]
  ]

tileFgColours :: [Texture]
tileFgColours =
  [ [rgba|776e65|], [rgba|776e65|], [rgba|f9f6f2|], [rgba|f9f6f2|]
  , [rgba|f9f6f2|], [rgba|f9f6f2|], [rgba|f9f6f2|], [rgba|f9f6f2|]
  , [rgba|f9f6f2|], [rgba|f9f6f2|], [rgba|f9f6f2|], [rgba|f9f6f2|]
  ]

tileFillColourOf :: HasConfig c => Int -> c -> Texture
tileFillColourOf l cfg = view tileFillColour cfg !! l

tileTextColourOf :: HasConfig c => Int -> c -> Texture
tileTextColourOf l cfg = view tileTextColour cfg !! l
