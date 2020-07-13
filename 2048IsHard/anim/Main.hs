{-# LANGUAGE OverloadedStrings #-}  
module Main where

import Control.Lens
import Control.Monad.State.Class

import Reanimate
import A2048.Config
import A2048.Board
import A2048.Logic
import A2048.Motion

config :: Game2048Config
config = defaultGame2048Config
  & boardWidth .~ 5
  & boardHeight .~ 3

main :: IO ()
main = reanimate $ gameAnimation config $ do
  put [[1, 2, 3, 4, 0]
      ,[1, 2, 3, 4, 0]
      ,[9, 10, 11, 12, 0]]
  hold 1
  performMove Up
  hold 1
