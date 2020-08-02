{-|
Module      : A3SAT.Expression
Description : Expressions for satisfiability problems.
Copyright   : (c) Xie Ruifeng, 2020
License     : AGPL-3
Maintainer  : krantz.xrf@outlook.com
Stability   : experimental
Portability : portable
-}
{-# LANGUAGE TemplateHaskell #-}
module A3SAT.Expression where

import Data.List
import Data.Bifunctor
import Data.Bifunctor.Fix

import Data.Bifunctor.TH
import Data.Bitraversable
import Data.Eq.Deriving
import Data.Ord.Deriving
import Text.Show.Deriving

-- * Expression Functors

-- ** Expression Functors with Labels

-- |The endofunctor for boolean expressions. 
data ExpressionF a l
  = LVarF l Word
  | LNotF l a
  | LAndF a l a
  | LOrF a l a
  | LParenF l a l
  deriving stock (Show, Eq, Functor, Foldable, Traversable)

deriveBifunctor ''ExpressionF
deriveBifoldable ''ExpressionF
deriveBitraversable ''ExpressionF

deriveEq2 ''ExpressionF
deriveOrd2 ''ExpressionF
deriveShow2 ''ExpressionF

-- |Given number of glyphs for variables, get the number of glyphs for this element.
glyphCount :: [Int] -> ExpressionF a l -> Int
glyphCount g (LVarF _ x) = g `genericIndex` x
glyphCount _ (LNotF _ _) = 1
glyphCount _ (LAndF _ _ _) = 1
glyphCount _ (LOrF _ _ _) = 1
glyphCount _ (LParenF _ _ _) = 1

-- ** Expression Constructors with Labels

pattern LVar :: l -> Word -> LExpression l
pattern LVar l x = In (LVarF l x)

pattern LNot :: l -> LExpression l -> LExpression l
pattern LNot l a = In (LNotF l a)

pattern LAnd, LOr :: LExpression l -> l -> LExpression l -> LExpression l
pattern LAnd a l b = In (LAndF a l b)
pattern LOr a l b = In (LOrF a l b)

pattern LParen :: l -> LExpression l -> l -> LExpression l
pattern LParen l a r = In (LParenF l a r)

{-# COMPLETE LVar, LNot, LAnd, LOr, LParen #-}

-- ** Expression Constructors without Labels

pattern Var :: Word -> Expression
pattern Var x = LVar () x

pattern Not, Paren :: Expression -> Expression
pattern Not a = LNot () a
pattern Paren a = LParen () a ()

pattern And, Or :: Expression -> Expression -> Expression
pattern And a b = LAnd a () b
pattern Or a b = LOr a () b

{-# COMPLETE Var, Not, And, Or, Paren #-}

infixr 2 `Or`
infixr 3 `And`

-- * Boolean Expressions

-- |Boolean expressions with labels.
type LExpression = Fix ExpressionF

-- |Boolean expressions.
type Expression = LExpression ()

-- * Utility Functions

-- |Add necessary parentheses for the boolean expression.
-- Parentheses labeled with the provided label.
addParenWith :: (l, l) -> LExpression l -> LExpression l
addParenWith (l, r) = go (0 :: Int) where
  go p e@(priority . out -> p')
    | p' <= p = In (LParenF l e' r)
    | otherwise = e'
    where e' = In $ first (go p') $ out e
  priority (LVarF _ _) = 4
  priority (LNotF _ _) = 2
  priority (LAndF _ _ _) = 1
  priority (LOrF _ _ _) = 1
  priority (LParenF _ _ _) = 3

-- |Add necessary parentheses for the boolean expression.
addParen :: Expression -> Expression
addParen = addParenWith ((), ())

-- |Transform the expression with the given visitor.
mapExprM :: Applicative f
         => (forall a b . ExpressionF a b -> c -> f d)
         -- ^Expression visitor: expression type -> old label -> new label.
         -> LExpression c
         -> f (LExpression d)
mapExprM convLabel = go where
  go (In e) = In <$> bimapM go (convLabel e) e
