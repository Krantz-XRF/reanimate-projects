{-# LANGUAGE OverloadedStrings #-}
module Intro (intro) where

import Prelude hiding (Left, Right)

import Control.Monad.Reader.Class
import Control.Monad.State.Class

import Reanimate

import Common.Animation.Effects

import A2048.Board
import A2048.Config
import A2048.Logic
import A2048.Motion
import A2048.Text
import A2048.Tile

config :: Game2048Config
config = defaultGame2048Config

-- |Video intro.
intro :: Animation
intro = mapA addWhiteBkg . fadeToEnd 1
  $ gameAnimation config $ do
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
  sequential <$> sequence
    [ pure (pause 0.5)
    , applyE fadeInE <$> hold 1
    , sequential <$> mapM performMove
      [ Left, Up,    Left, Up,   Down
      , Left, Right, Up,   Left, Left ]
    , hold 0.5
    , do (x0, y0) <- asks (`gridPos` (0, 0))
         tl <- tile 11
         bd <- boardSVG
         let trans t = translate (lerp t (-2) x0) (lerp t 0 y0)
         let animTl = animate (`trans` tl)
         let fadeBoard = setDuration 0.5 $ applyE fadeOutE $ staticFrame 1 bd
         let mv = setDuration 2 (animTl `parA` fadePrompt) `parA` fadeBoard
         pure (mv `andThen` pause 2) ]
