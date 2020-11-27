{-|
Module      : A3SAT.Animation.Config
Description : Rendering configuration for 3-SAT problems.
Copyright   : (c) Xie Ruifeng, 2020
License     : AGPL-3-or-later
Maintainer  : krantz.xrf@outlook.com
Stability   : experimental
Portability : portable
-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE Strict          #-}
{-# LANGUAGE TemplateHaskell #-}
module A3SAT.Animation.Config where

import Data.List

import Common.HexColour
import Control.Lens
import Graphics.SvgTree

-- |How do we colour the variables?
data ColourStrategy
  -- |Use 'defaultTextColour'.
  = NoColour
  -- |Use corresponding 'varColours'.
  | VarColour
  -- |Use 'trueColour' and 'falseColour'.
  -- Provided the boolean values for those variables.
  | BoolColour [Bool]
  deriving stock (Show, Eq)

-- |Configurations for the 2048 Game.
data Anim3SATConfig = Anim3SATConfig
  { _defaultTextColour :: Texture
  , _operatorColour    :: Texture
  , _varColours        :: [Texture]
  , _colourStrategy    :: ColourStrategy
  , _trueColour        :: Texture
  , _falseColour       :: Texture
  } deriving stock (Show)

-- |Classy lens for 'Anim3SATConfig'.
makeClassy ''Anim3SATConfig

-- |Get variable colour from the 'Anim3SATConfig'.
getVarColour :: HasAnim3SATConfig c => c -> Word -> Texture
getVarColour cfg n = case cfg ^. colourStrategy of
  NoColour  -> cfg ^. defaultTextColour
  VarColour -> (cfg ^. varColours) `genericIndex` n
  BoolColour bs -> if bs `genericIndex` n
    then cfg ^. trueColour
    else cfg ^. falseColour

-- |Get variable colour from the 'Anim3SATConfig'.
getVarColours :: HasAnim3SATConfig c => c -> [Texture]
getVarColours cfg = map (getVarColour cfg) [0 ..]

-- |Default configuration for tiles.
defaultAnim3SATConfig :: Anim3SATConfig
defaultAnim3SATConfig = Anim3SATConfig
  { _defaultTextColour = [rgba|000000|]
  , _operatorColour    = [rgba|424242|]
  , _varColours        = defaultVarColours
  , _colourStrategy    = NoColour
  , _trueColour        = [rgba|99DD99|]
  , _falseColour       = [rgba|EAEAEA|]
  }

-- |Default variable colours.
defaultVarColours :: [Texture]
defaultVarColours
  = [rgba|DDCC44|]
  : [rgba|77BBEE|]
  : [rgba|6E6EEE|]
  : [rgba|DD9999|]
  : [rgba|BB1E1E|]
  : repeat [rgba|000000|]
