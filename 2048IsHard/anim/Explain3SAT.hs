module Explain3SAT (explain3SAT) where

import qualified Data.Text as T

import Reanimate

import Common.Animation.Effects

import A3SAT.Animation.Config
import A3SAT.Animation.Effects
import A3SAT.Expression

config :: Anim3SATConfig
config = defaultAnim3SATConfig

expr :: Expression
expr = addParen $
        (Not (Var 0) `Or` Not (Var 1) `Or` Var 3)
  `And` (Not (Var 0) `Or` Var 1 `Or` Not (Var 2))
  `And` (Var 0 `Or` Var 1 `Or` Not (Var 3))

varNames :: [T.Text]
varNames = map (T.pack . ("\\bm{x_" <>) . (<> "}") . show) [0 :: Int ..]

-- |Animation for explaining the 3-SAT problem.
explain3SAT :: Animation
explain3SAT = mapA (addWhiteBkg . scale 0.5)
  $ exprAnimation config
  $ sequential <$> sequence
  [ holdExpr varNames expr 1
  , highlightVars varNames expr
  , holdExprWith VarColour varNames expr 1 ]
