{-# LANGUAGE OverloadedStrings #-}  
module Main where

import Control.Lens

import Reanimate
import A2048.Config
import A2048.Board

config :: Game2048Config
config = defaultGame2048Config
  & boardWidth .~ 5
  & boardHeight .~ 3

main :: IO ()
main = reanimate $ gameAnimation config $ do
  putBoard [[1, 2, 3, 4, 0]
           ,[5, 6, 7, 8, 0]
           ,[9, 10, 11, 12, 0]]
  hold 1
