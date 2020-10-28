module Main where

import Control.Monad
import System.Environment
import Text.Printf

import Reanimate

import Explain3SAT
import ExplainLog
import Intro

anims :: [(String, Animation)]
anims =
  [("Intro",       intro)
  ,("ExplainLog",  explainLog)
  ,("Explain3SAT", explain3SAT)]

printAnims :: IO ()
printAnims = forM_ (indexedAnims) $ \(n, (nm, _)) -> printf "%d. %s\n" n nm
  where indexedAnims = zip [1 :: Int ..] anims

findAnim :: String -> Maybe Animation
findAnim x = lookup x anims

handleArgs :: [String] -> IO () -> (String -> Animation -> IO ()) -> IO ()
handleArgs [x] nil k = maybe nil (k x) (findAnim x)
handleArgs _   nil _ = nil

chooseAnim :: (String -> Animation -> IO ()) -> IO ()
chooseAnim k = do
  args <- getArgs
  handleArgs args printAnims k

main :: IO ()
main = chooseAnim $ \nm anim -> withArgs
  [ "render"
  , "--preset", "high"
  , "-o", nm ++ ".mp4" ]
  (reanimate anim)
