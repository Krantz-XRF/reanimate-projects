{-# LANGUAGE OverloadedStrings #-}  
module Main where

import Control.Monad.State.Class

import Reanimate
import A2048.Config
import A2048.Board

main :: IO ()
main = reanimate $ gameAnimation defaultBoardConfig $ do
  put [[1, 2, 3, 4]
      ,[5, 6, 7, 8]
      ,[9, 10, 11, 12]
      ,[0, 0, 0, 0]]
  hold 1
