module Main where

import Reanimate          (reanimate)
import System.Environment (withArgs)
import TypeCheck          (typeCheckAnim)

main :: IO ()
main = withArgs
  [ "render"
  , "--preset", "high"
  , "-o", "TypeCheckKaomoji.mp4" ]
  (reanimate typeCheckAnim)
