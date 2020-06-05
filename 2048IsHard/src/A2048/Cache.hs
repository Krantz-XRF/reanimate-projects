{-|
Module      : A2048.Cache
Description : Cache support for 2048 rendering.
Copyright   : (c) Xie Ruifeng, 2020
License     : AGPL-3
Maintainer  : krantz.xrf@outlook.com
Stability   : experimental
Portability : portable
-}
{-# LANGUAGE DeriveAnyClass #-}
module A2048.Cache where

import GHC.Generics
import Data.Hashable

data A2048Cache
  = BoardBGCache !Int !Int
  | BoardStatusCache [[Int]]
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Hashable)
