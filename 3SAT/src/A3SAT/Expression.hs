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

import Data.Bifunctor
import Data.Bifunctor.Fix
import Data.List

import Data.Bifunctor.TH
import Data.Bitraversable
import Data.Eq.Deriving
import Data.Ord.Deriving
import Text.Show.Deriving

-- * Expression Functors

-- ** Expression Functors with Labels

-- |The endofunctor for boolean expressions.
data ExpressionF a l
  = VarF l Word
  | NotF l a
  | AndF a l a
  | OrF a l a
  | ParenF l a l
  deriving stock (Show, Eq, Functor, Foldable, Traversable)

deriveBifunctor ''ExpressionF
deriveBifoldable ''ExpressionF
deriveBitraversable ''ExpressionF

deriveEq2 ''ExpressionF
deriveOrd2 ''ExpressionF
deriveShow2 ''ExpressionF

-- |Given number of glyphs for variables, get the number of glyphs for this element.
glyphCount :: [Int] -> ExpressionF a l -> Int
glyphCount g (VarF _ x)     = g `genericIndex` x
glyphCount _ (NotF _ _)     = 1
glyphCount _ (AndF _ _ _)   = 1
glyphCount _ (OrF _ _ _)    = 1
glyphCount _ (ParenF _ _ _) = 1

-- ** Expression Constructors with Labels

-- |Labeled Variable.
pattern LVar :: l -> Word -> LExpression l
pattern LVar l x = In (VarF l x)

-- |Boolean negation with label.
pattern LNot :: l -> LExpression l -> LExpression l
pattern LNot l a = In (NotF l a)

-- |Boolean and with label.
pattern LAnd :: LExpression l -> l -> LExpression l -> LExpression l
pattern LAnd a l b = In (AndF a l b)

-- |Boolean or with label.
pattern LOr :: LExpression l -> l -> LExpression l -> LExpression l
pattern LOr a l b = In (OrF a l b)

-- |Parenthesis with left and right labels.
pattern LParen :: l -> LExpression l -> l -> LExpression l
pattern LParen l a r = In (ParenF l a r)

{-# COMPLETE LVar, LNot, LAnd, LOr, LParen #-}

-- ** Expression Constructors without Labels

-- |Variable with a dummy label.
pattern Var :: Word -> Expression
pattern Var x = LVar () x

-- |Boolean negation with a dummy label.
pattern Not :: Expression -> Expression
pattern Not a = LNot () a

-- |Boolean and with a dummy label.
pattern And :: Expression -> Expression -> Expression
pattern And a b = LAnd a () b

-- |Boolean or with a dummy label.
pattern Or :: Expression -> Expression -> Expression
pattern Or a b = LOr a () b

-- |Parenthesis with dummy labels.
pattern Paren :: Expression -> Expression
pattern Paren a = LParen () a ()

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
    | p' <= p = In (ParenF l e' r)
    | otherwise = e'
    where e' = In $ first (go p') $ out e
  priority (VarF _ _)     = 4
  priority (NotF _ _)     = 2
  priority (AndF _ _ _)   = 1
  priority (OrF _ _ _)    = 1
  priority (ParenF _ _ _) = 3

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

-- |Zip 2 expression functors with of same shape with given functions.
zipExprFWith :: (a -> a' -> a'') -> (l -> l' -> l'')
             -> ExpressionF a l -> ExpressionF a' l' -> ExpressionF a'' l''
zipExprFWith _ g (VarF l1 x)       (VarF l2 _)       = VarF (g l1 l2) x
zipExprFWith f g (NotF l1 a1)      (NotF l2 a2)      = NotF (g l1 l2) (f a1 a2)
zipExprFWith f g (AndF a1 l1 b1)   (AndF a2 l2 b2)   = AndF (f a1 a2) (g l1 l2) (f b1 b2)
zipExprFWith f g (OrF a1 l1 b1)    (OrF a2 l2 b2)    = OrF (f a1 a2) (g l1 l2) (f b1 b2)
zipExprFWith f g (ParenF l1 a1 r1) (ParenF l2 a2 r2) = ParenF (g l1 l2) (f a1 a2) (g r1 r2)
zipExprFWith _ _ _ _ = error "zipExprFWith: requires input of same shape"

-- |Zip 2 expression functors of the same shape.
zipExprF :: ExpressionF a l -> ExpressionF a' l' -> ExpressionF (a, a') (l, l')
zipExprF = zipExprFWith (,) (,)

-- |Zip 2 expressions of the same shape with given function.
zipExprWith :: (l -> l' -> l'') -> LExpression l -> LExpression l' -> LExpression l''
zipExprWith f = go where go (In x) (In y) = In (zipExprFWith go f x y)

-- |Zip 2 expressions of the same shape with given function.
zipExpr :: LExpression l -> LExpression l' -> LExpression (l, l')
zipExpr = zipExprWith (,)
