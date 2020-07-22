{-# LANGUAGE OverloadedStrings #-}  
module Main where

import Prelude hiding (Left, Right)

import Control.Monad.State.Class

import Reanimate
import A2048.Config
import A2048.Board
import A2048.Logic
import A2048.Motion

config :: Game2048Config
config = defaultGame2048Config

main :: IO ()
main = reanimate $ gameAnimation config $ do
  put [[3,  0, 1, 1]
      ,[0,  4, 0, 0]
      ,[5,  2, 6, 7]
      ,[10, 9, 0, 8]]
  foldl seqA (pause 0) <$> sequence
    [ hold 1
    , foldl seqA (pause 0) <$> mapM performMove
      [ Left, Up,    Left, Up,   Down
      , Left, Right, Up,   Left, Left ]
    , hold 1 ]
