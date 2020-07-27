{-# LANGUAGE OverloadedStrings #-}  
module ExplainLog (explainLog) where

import Prelude hiding (Left, Right)

import Control.Monad.State.Class

import Reanimate

import Anim.Common

import A2048.Config
import A2048.Board
import A2048.SwitchLogarithm

config :: Game2048Config
config = defaultGame2048Config{ _boardHeight = 3 }

-- |Animation for explaining the use of logarithm.
explainLog :: Animation
explainLog = gameAnimation config
  $ mapA addWhiteBkg . fadeToEnd 1 <$> do
  put [[1,  2,  3, 4]
      ,[5,  6,  7, 8]
      ,[9, 10, 11, 0]]
  foldl seqA (pause 0) <$> sequence
    [ applyE fadeInE <$> hold 1
    , setDuration 2 <$> switchLog ]
