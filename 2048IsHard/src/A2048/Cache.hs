{-|
Module      : A2048.Cache
Description : Cache support for 2048 rendering.
Copyright   : (c) Xie Ruifeng, 2020
License     : AGPL-3
Maintainer  : krantz.xrf@outlook.com
Stability   : experimental
Portability : portable
-}
module A2048.Cache where

import GHC.Generics
import Data.Hashable
import Control.Lens

import A2048.Config

-- |Cache ID for 2048 game scene.
data A2048CacheId
  = TileCache !Int !Double
  | BoardBGCache !Int !Int !Double
  | BoardStatusCache [[Int]]
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable)

-- |Board background cache ID, assuming resize happens coherently.
bgCacheId :: HasGame2048Config c => c -> A2048CacheId
bgCacheId cfg =
  let c = view game2048Config cfg
  in BoardBGCache (view boardWidth c) (view boardHeight c) (view tileSize c)

-- |Tile cache ID, assuming resize happens coherently.
tileCacheId :: Int -> Game2048Config -> A2048CacheId
tileCacheId x cfg = TileCache x (view (game2048Config . tileSize) cfg)
