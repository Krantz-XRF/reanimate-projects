{-# LANGUAGE OverloadedStrings #-}  
module Main where

import Reanimate

main :: IO ()
main = reanimate
     $ addStatic (mkBackground "cyan")
     $ staticFrame 1
     $ mkText "Hello world"
