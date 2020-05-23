{-|
Module      : Control.Monad.ST.Class
Description : The missing MonadST class in mtl.
Copyright   : (c) Xie Ruifeng, 2020
License     : AGPL-3
Maintainer  : krantz.xrf@outlook.com
Stability   : experimental
Portability : portable
-}
{-# LANGUAGE TypeFamilies #-}
module Control.Monad.ST.Class
  ( module Control.Monad.ST
  , MonadST(..)
  ) where

import Control.Monad.ST
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer.CPS

-- |Lift a 'ST' monad up a deep monad transformer stack.
class Monad m => MonadST m where
  type WorldType m :: *
  liftST :: ST (WorldType m) a -> m a

instance MonadST (ST s) where
  type WorldType (ST s) = s
  liftST = id

instance MonadST m => MonadST (ReaderT a m) where
  type WorldType (ReaderT a m) = WorldType m
  liftST = lift . liftST

instance MonadST m => MonadST (WriterT w m) where
  type WorldType (WriterT w m) = WorldType m
  liftST = lift . liftST

instance MonadST m => MonadST (StateT s m) where
  type WorldType (StateT s m) = WorldType m
  liftST = lift . liftST
