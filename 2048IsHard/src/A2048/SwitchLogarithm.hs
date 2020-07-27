{-|
Module      : A2048.SwitchLogarithm
Description : Animation for switching config 'useLogarithm'.
Copyright   : (c) Xie Ruifeng, 2020
License     : AGPL-3
Maintainer  : krantz.xrf@outlook.com
Stability   : experimental
Portability : portable
-}
module A2048.SwitchLogarithm
  ( switchLog
  , localLog
  ) where

import Control.Monad.Reader.Class

import Reanimate
import Reanimate.Morph.Common
import Reanimate.Morph.Linear

import A2048.Config
import A2048.Tile
import A2048.Board

-- |Use logarithm locally.
localLog :: Monad2048 m => m a -> m a
localLog = local switch where
  switch cfg = cfg{ _useLogarithm = True }

localHideLabel :: Monad2048 m => m a -> m a
localHideLabel = local switch where
  switch cfg = cfg{ _tileShowLabel = False }

-- |Animation for the board from 2-4-8-16 to 1-2-3-4.
switchLog :: Monad2048 m => m Animation
switchLog = do
  bd <- localHideLabel snapshot
  anim <- mkPure $ \t -> foreachNonEmptyGrid $ \n -> do
    l <- lowerTransformations . pathify <$> tileLabel n
    l' <- lowerTransformations . pathify <$> localLog (tileLabel n)
    pure (morph linear l l' t)
  pure (animate $ mkGroup . (bd :) . anim)
