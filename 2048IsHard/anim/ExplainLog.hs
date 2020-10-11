{-# LANGUAGE OverloadedStrings #-}
module ExplainLog (explainLog) where

import Prelude hiding (Left, Right)

import Control.Monad.State.Class

import Reanimate

import Common.Animation.Effects

import A2048.Board
import A2048.Config
import A2048.SwitchLogarithm

config :: Game2048Config
config = defaultGame2048Config{ _boardHeight = 3 }

-- |Animation for explaining the use of logarithm.
explainLog :: Animation
explainLog = mapA addWhiteBkg . fadeToEnd 1
  $ gameAnimation config $ do
  put [[1,  2,  3, 4]
      ,[5,  6,  7, 8]
      ,[9, 10, 11, 0]]
  sequential <$> sequence
    [ applyE fadeInE <$> hold 1
    , setDuration 0.5 <$> switchPlainExpo
    , holdWith Exponent 1
    , expoHighlight
    , holdWith Exponent 0.5
    , switchExpoLog
    , holdWith Logarithm 1 ]
