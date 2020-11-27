{-|
Module      : Common.Bifunctor
Description : Common utilities for bifunctors.
Copyright   : (c) Xie Ruifeng, 2020
License     : AGPL-3-or-later
Maintainer  : krantz.xrf@outlook.com
Stability   : experimental
Portability : portable
-}
module Common.Bifunctor where

import Control.Lens
import Data.Bifunctor
import Data.Bifunctor.Fix

-- |Fold 'Fix'. Copied from data-fix 0.3.0.
-- Package "data-fix" uses 'Functor' instead of 'Bifunctor'.
foldFix :: Bifunctor f => (f b a -> b) -> Fix f a -> b
foldFix f = go where go = f . first go . out

-- |Scan 'Fix'.
scanFix :: Bifunctor f => (f b a -> b) -> Fix f a -> Fix f b
scanFix = scanFixIdx id

-- |Scan 'Fix' with 'Control.Lens.Type.Lens'.
scanFixIdx :: Bifunctor f => Lens s t a b -> (f b a -> b) -> Fix f s -> Fix f t
scanFixIdx idx f = snd . go where
  go (In x) =
    let p = first go x
        b = f (bimap fst (get idx) p)
        get = view . getting
    in (b, In (bimap snd (set idx b) p))
