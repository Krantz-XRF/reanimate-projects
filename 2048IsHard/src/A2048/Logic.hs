{-|
Module      : A2048.Logic
Description : 2048 game logic.
Copyright   : (c) Xie Ruifeng, 2020
License     : AGPL-3-or-later
Maintainer  : krantz.xrf@outlook.com
Stability   : experimental
Portability : portable
-}
module A2048.Logic where

import Prelude hiding (Left, Right)

import Data.List
import Data.Bifunctor

-- |Game Events, parameterised by value and position.
data GameEvent a p
  -- |This tile is merged from two tiles, original positions provided.
  = TileMerge a p p
  -- |This tile is merely moving, original position provided.
  | TileMove a p
  -- |Placeholder, here no tile exists.
  | TileVanish
  deriving stock (Show, Functor)

instance Bifunctor GameEvent where
  bimap f g (TileMerge a p1 p2) = TileMerge (f a) (g p1) (g p2)
  bimap f g (TileMove a p)      = TileMove (f a) (g p)
  bimap _ _ TileVanish          = TileVanish

-- |Merge a row, 'Nothing' as empty tiles.
rowMerge :: (Eq a, Enum a) => [Maybe a] -> [GameEvent a Int]
rowMerge = go [] . zipWith pair [0 ..] where
  pair p = fmap (, p)
  go ts [] = ts
  go ts (Nothing : xs) = go (TileVanish : ts) xs
  go ts (x : Nothing : xs) = go (TileVanish : ts) (x : xs)
  go ts (Just (x, px) : Just (y, py) : xs) | x == y
    = TileMerge (succ x) px py : go (TileVanish : ts) xs
  go ts (Just (x, px) : xs) = TileMove x px : go ts xs

-- |Merge a whole board, 'Nothing' as empty tiles.
boardMerge :: (Eq a, Enum a) => [[Maybe a]] -> [[GameEvent a (Int, Int)]]
boardMerge = zipWith markRow [0 ..] . map rowMerge where
  markRow r = map (second (, r))

-- |4 directions, for game moves.
data Direction = Left | Right | Up | Down
  deriving stock (Show, Enum, Eq, Ord)

-- |Transform indices according to a 'Direction'.
-- This @addIndices@ pairs each element with an index:
--
-- > addIndices xs =
-- >   [ [ (x, y, a)
-- >     | (x, a) <- zip [0 ..] row]
-- >   | (y, row) <- zip [0 ..] xs]
--
-- And this @transIndices@ use 'idxTrans' to transform indices:
--
-- > transIndices = map (map (idxTrans w h d))
--
-- Then we have the following property:
--
-- prop> rotateTo d . addIndices == transIndices . addIndices . rotateTo d
--
-- 'idxTrans' transforms the index as if it were added before a 'rotateTo'.
idxTrans :: Int -> Int -> Direction -> (Int, Int) -> (Int, Int)
idxTrans _ _ Left  (x, y) = (x, y)
idxTrans w _ Right (x, y) = (w - x - 1, y)
idxTrans _ _ Up    (x, y) = (y, x)
idxTrans _ h Down  (x, y) = (y, h - x - 1)

-- |Rotate game board according to a 'Direction'.
rotateTo :: Direction -> [[a]] -> [[a]]
rotateTo Left  = id
rotateTo Right = map reverse
rotateTo Up    = transpose
rotateTo Down  = map reverse . transpose

-- |Recover from a rotated game board according to a 'Direction'.
rotateFrom :: Direction -> [[a]] -> [[a]]
rotateFrom Left  = id
rotateFrom Right = map reverse
rotateFrom Up    = transpose
rotateFrom Down  = transpose . map reverse

-- |Gather game events on a game move to a given 'Direction'.
events :: Int -> Int -> Direction -> [[Int]] -> [[GameEvent Int (Int, Int)]]
events w h d = rotated (transIndices . boardMerge) . mapInput where
  mapInput = mapBoard $ \case 0 -> Nothing; n -> Just n
  transIndices = mapBoard $ second $ idxTrans w h d
  rotated f = rotateFrom d . f . rotateTo d
  mapBoard f = map (map f)
