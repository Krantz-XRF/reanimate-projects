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
  = BoardBGCache !Int !Int !Double
  | BoardStatusCache [[Int]]
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable)

-- |Board background cache ID, assuming resize happens coherently.
bgCacheId :: Game2048Config -> A2048CacheId
bgCacheId cfg = BoardBGCache
  (view boardWidth cfg)
  (view boardHeight cfg)
  (view tileSize cfg)
