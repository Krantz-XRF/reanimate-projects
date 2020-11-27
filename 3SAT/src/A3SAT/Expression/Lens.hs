{-|
Module      : A3SAT.Expression.Lens
Description : Lens for the 'ExpressionF' functor and 'LExpression's.
Copyright   : (c) Xie Ruifeng, 2020
License     : AGPL-3-or-later
Maintainer  : krantz.xrf@outlook.com
Stability   : experimental
Portability : portable
-}
{-# LANGUAGE TemplateHaskell #-}
module A3SAT.Expression.Lens where

import A3SAT.Expression
import Control.Lens
import Control.Lens.Extras (is)
import Data.Bifunctor.Fix
import Data.Bitraversable

-- * Optics for Expression Functor

makePrisms ''ExpressionF

-- |Prisms for 'AndF' and 'OrF'.
_BinOpF :: Traversal' (ExpressionF a l) (a, l, a)
_BinOpF = disjointUnion _AndF _OrF

-- |Label for variables matching the predicate.
varF :: (Word -> Bool) -> Traversal' (ExpressionF a l) (l, Word)
varF f = _VarF . filtered (f . view _2)

-- |Label for all variables.
allVarF :: Traversal' (ExpressionF a l) l
allVarF = varF (const True) . _1

-- |Label for 'NotF'.
notF :: Traversal' (ExpressionF a l) l
notF = _NotF . _1

-- |Label for 'AndF'.
andF :: Traversal' (ExpressionF a l) l
andF = _AndF . _2

-- |Label for 'OrF'.
orF :: Traversal' (ExpressionF a l) l
orF = _OrF . _2

-- |Label for 'AndF' and 'OrF'.
binOpF :: Traversal' (ExpressionF a l) l
binOpF = _BinOpF . _2

-- |Left parentheses.
leftParenF :: Traversal' (ExpressionF a l) l
leftParenF = _ParenF . _1

-- |Right parentheses.
rightParenF :: Traversal' (ExpressionF a l) l
rightParenF = _ParenF . _3

-- |Both parentheses.
parenF :: Traversal' (ExpressionF a l) l
parenF = _ParenF . _1_and_3 where
  _1_and_3 f (x, a, y) = (,,) <$> f x <*> pure a <*> f y

-- * Optics for Expressions

-- |Label for variables matching the predicate.
vars :: (Word -> Bool) -> Traversal' (LExpression l) (l, Word)
vars f = fixExpr (varF f)

-- |Label for all variables.
allVars :: Traversal' (LExpression l) l
allVars = vars (const True) . _1

-- |Label for 'NotF'.
nots :: Traversal' (LExpression l) l
nots = fixExpr notF

-- |Label for 'AndF'.
ands :: Traversal' (LExpression l) l
ands = fixExpr andF

-- |Label for 'OrF'.
ors :: Traversal' (LExpression l) l
ors = fixExpr orF

-- |Label for 'AndF' and 'OrF'.
binOps :: Traversal' (LExpression l) l
binOps = fixExpr binOpF

-- |Left parentheses.
leftParens :: Traversal' (LExpression l) l
leftParens = fixExpr leftParenF

-- |Right parentheses.
rightParens :: Traversal' (LExpression l) l
rightParens = fixExpr rightParenF

-- |Both parentheses.
parens :: Traversal' (LExpression l) l
parens = fixExpr parenF

-- * "Global" Optics on Expressions

-- |Travel through a parenthesis.
transParen :: Traversal' (LExpression l) (LExpression l)
transParen f (LParen l x r) = LParen <$> pure l <*> transParen f x <*> pure r
transParen f z              = f z

-- |An 'LExpression' as a product of a list of 'LExpression's.
asProduct :: Traversal' (LExpression l) (LExpression l)
asProduct f (LAnd x l y) = LAnd <$> asProduct f x <*> pure l <*> asProduct f y
asProduct f z            = f z

-- |An 'LExpression' as a sum of a list of 'LExpression's.
asSum :: Traversal' (LExpression l) (LExpression l)
asSum f (LOr x l y) = LOr <$> asSum f x <*> pure l <*> asSum f y
asSum f z           = f z

-- |The "primary" label for an 'LExpression'.
primaryLabel :: Lens' (LExpression l) l
primaryLabel f (LVar l x)      = f l <&> \a -> LVar a x
primaryLabel f (LNot l x)      = f l <&> \a -> LNot a x
primaryLabel f (LAnd x l y)    = f l <&> \a -> LAnd x a y
primaryLabel f (LOr x l y)     = f l <&> \a -> LOr x a y
primaryLabel f (LParen l x l') = f l <&> \a -> LParen a x l'

-- * Utility Functions

-- |Make a disjoint union of 2 'Prism''s.
disjointUnion :: Prism' s a -> Prism' s a -> Traversal' s a
disjointUnion l r f s = if is l s then l f s else r f s

-- |Fixpoint for lenses on the expression functor.
--
-- WARNING: recursive traversal are done after non-recursive ones.
-- Thus traversal does not follow the field order in data declaration.
fixExpr :: Applicative f
        => (forall a . LensLike' f (ExpressionF a l) b)
        -> LensLike' f (LExpression l) b
fixExpr ls f = go where
  go (In x) = In <$> (merge <$> ls f x <*> bitraverse go pure x)
  merge = zipExprFWith (const id) const (error "impossible")
