{-# LANGUAGE OverloadedStrings #-}  
module Main where

import Prelude hiding (Left, Right)

import Control.Monad.State.Class
import Control.Monad.Reader.Class

import Reanimate

import A2048.Config
import A2048.Tile
import A2048.Board
import A2048.Logic
import A2048.Motion
import A2048.Text

config :: Game2048Config
config = defaultGame2048Config

main :: IO ()
main = reanimate
  $ gameAnimation config
  $ mapA (\svg -> mkGroup [mkBackground "white", svg]) <$> do
  put [[3,  0, 1, 1]
      ,[0,  4, 0, 0]
      ,[5,  2, 6, 7]
      ,[10, 9, 0, 8]]
  let prompt = mkGroup [label "\\textbf{有多难？}"]
  let fadePrompt
        = signalA (curveS 2)
        $ applyE (translateE 1 0)
        $ applyE fadeInE
        $ staticFrame 1 prompt
  let lerp r a b = r * a + (1 - r) * b
  foldl seqA (pause 0.5) <$> sequence
    [ applyE fadeInE <$> hold 1
    , foldl seqA (pause 0) <$> mapM performMove
      [ Left, Up,    Left, Up,   Down
      , Left, Right, Up,   Left, Left ]
    , hold 0.5
    , do (x0, y0) <- asks (`gridPos` (0, 0))
         tl <- tile 11
         bd <- boardSVG
         let trans t = translate (lerp t (-2) x0) (lerp t 0 y0)
         let animTl = animate (`trans` tl)
         let fadeBoard = applyE fadeOutE (staticFrame 1 bd)
         let mv = setDuration 2 (animTl `parA` fadePrompt) `parA` fadeBoard
         pure (mv `andThen` pause 2) ]
