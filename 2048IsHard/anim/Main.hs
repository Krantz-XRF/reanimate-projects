{-# LANGUAGE OverloadedStrings #-}  
module Main where

import Control.Monad.Reader

import Reanimate
import A2048.Config
import A2048.Tile

main :: IO ()
main = reanimate
     $ addStatic (mkBackground "cyan")
     $ staticFrame 1
     $ runReader (tile 1) defaultConfig
