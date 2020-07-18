{-# LANGUAGE LambdaCase #-}
module Language where

import Control.Monad
import Numeric.Natural
import qualified Data.Map as M
import Control.Applicative

import Calculus

data Atom
  = Num Integer
  | Bits [Bool]
  | Picture Natural Natural [[Bool]]
  | Comb Combinator
  deriving (Eq, Ord, Show)

data Combinator
  = Id
  | CTrue
  | CFalse
  | S
  | Compose
  | Flip
  | Pair
  | Fst
  | Snd
  | Nil
  | IsNil

  | Add
  | Mul
  | Div
  | EqBool
  | Lt
  | Pow2
  | Neg
  | Succ
  | Pred
  | Choose

  | Mod
  | Dem
  | Eval
  | Point
  | MkImage
  | MkChecker
  | MapImage
  deriving (Eq, Ord, Show)

semantics :: Show v => [Rule Atom Integer v]
semantics =
  [ Rule
    { ruleName = "Succ"
    , ruleLHS = EAtom (Comb Succ) `EAp` EMVar 0
    , ruleRHS = EMVar 1
    , ruleProcess = unaryIntOp succ
    }
  , Rule
    { ruleName = "Pred"
    , ruleLHS = EAtom (Comb Pred) `EAp` EMVar 0
    , ruleRHS = EMVar 1
    , ruleProcess = unaryIntOp pred
    }
  , Rule
    { ruleName = "Add"
    , ruleLHS = EAtom (Comb Add) `EAp` EMVar 0 `EAp` EMVar 1
    , ruleRHS = EMVar 2
    , ruleProcess = binaryIntOp (+)
    }
  , Rule
    { ruleName = "Mul"
    , ruleLHS = EAtom (Comb Mul) `EAp` EMVar 0 `EAp` EMVar 1
    , ruleRHS = EMVar 2
    , ruleProcess = binaryIntOp (*)
    }
  , Rule
    { ruleName = "Div"
    , ruleLHS = EAtom (Comb Div) `EAp` EMVar 0 `EAp` EMVar 1
    , ruleRHS = EMVar 2
    , ruleProcess = binaryIntOp quot
    }
  , Rule
    { ruleName = "EqBool"
    , ruleLHS = EAtom (Comb EqBool) `EAp` EMVar 0 `EAp` EMVar 1
    , ruleRHS = EMVar 2
    , ruleProcess = NoShow $ \m -> do
        n <- getInteger =<< M.lookup 0 m
        m <- getInteger =<< M.lookup 1 m
        pure $ M.fromList [(2, EAtom $ Comb $ if n == m then CTrue else CFalse)]
    }
  , Rule
    { ruleName = "Lt"
    , ruleLHS = EAtom (Comb Lt) `EAp` EMVar 0
    , ruleRHS = EMVar 1
    , ruleProcess = NoShow $ \m -> do
        n <- getInteger =<< M.lookup 0 m
        m <- getInteger =<< M.lookup 1 m
        pure $ M.fromList [(2, EAtom $ Comb $ if n < m then CTrue else CFalse)]
    }
  , Rule
    { ruleName = "Neg"
    , ruleLHS = EAtom (Comb Neg) `EAp` EMVar 0
    , ruleRHS = EMVar 1
    , ruleProcess = NoShow $ \m -> do
        n <- getInteger =<< M.lookup 0 m
        m <- getInteger =<< M.lookup 1 m
        pure $ M.fromList [(2, EAtom $ Comb $ if n < m then CTrue else CFalse)]
    }
  , Rule
    { ruleName = "MkImage"
    , ruleLHS = EAtom (Comb MkImage) `EAp` EMVar 0
    , ruleRHS = EMVar 1
    , ruleProcess = NoShow $ \m -> do
        let natPair p = do
              (i, j) <- getPair p
              x <- getInteger i
              y <- getInteger j
              guard (x >= 0 && y >= 0)
              pure (fromInteger x, fromInteger y)
        pixels <- mapM natPair =<< getList =<< M.lookup 0 m
        let { width = 17; height = 13 }
        let contents = [[(x, y) `elem` pixels | x <- [0..width-1]] | y <- [0..height-1]]
        pure $ M.fromList [(1, EAtom $ Picture width height contents)]
    }

  , Rule
    { ruleName = "<*>"
    , ruleLHS = EAp (EAp (EAp (EAtom (Comb S)) (EMVar 0)) (EMVar 1)) (EMVar 2)
    , ruleRHS = EAp (EAp (EMVar 0) (EMVar 2)) (EAp (EMVar 1) (EMVar 2))
    , ruleProcess = NoShow Just
    }
  , Rule
    { ruleName = "Flip"
    , ruleLHS = EAp (EAp (EAp (EAtom (Comb Flip)) (EMVar 0)) (EMVar 1)) (EMVar 2)
    , ruleRHS = EAp (EAp (EMVar 0) (EMVar 2)) (EMVar 1)
    , ruleProcess = NoShow Just
    }
  , Rule
    { ruleName = "Compose"
    , ruleLHS = EAp (EAp (EAp (EAtom (Comb Compose)) (EMVar 0)) (EMVar 1)) (EMVar 2)
    , ruleRHS = EAp (EMVar 0) (EAp (EMVar 1) (EMVar 2))
    , ruleProcess = NoShow Just
    }
  , Rule
    { ruleName = "True"
    , ruleLHS = EAp (EAp (EAtom (Comb CTrue)) (EMVar 0)) (EMVar 1)
    , ruleRHS = EMVar 0
    , ruleProcess = NoShow Just
    }
  , Rule
    { ruleName = "False"
    , ruleLHS = EAp (EAp (EAtom (Comb CFalse)) (EMVar 0)) (EMVar 1)
    , ruleRHS = EMVar 1
    , ruleProcess = NoShow Just
    }
  , Rule
    { ruleName = "Pow2"
    , ruleLHS = EAtom (Comb Pow2)
    , ruleRHS = EAp (EAp (EAtom (Comb S)) (EAp (EAp (EAtom (Comb Flip)) (EAp (EAtom (Comb EqBool)) (EAtom (Num 0)))) (EAtom (Num 1)))) (EAp (EAp (EAtom (Comb Compose)) (EAp (EAtom (Comb Mul)) (EAtom (Num 2)))) (EAp (EAp (EAtom (Comb Compose)) (EAtom (Comb Pow2))) (EAp (EAtom (Comb Add)) (EAtom (Num (-1))))))
    , ruleProcess = NoShow Just
    }
  , Rule
    { ruleName = "Id"
    , ruleLHS = EAp (EAtom (Comb Id)) (EMVar 0)
    , ruleRHS = EMVar 0
    , ruleProcess = NoShow Just
    }
  , Rule
    { ruleName = "Pair"
    , ruleLHS = EAp (EAp (EAp (EAtom (Comb Pair)) (EMVar 0)) (EMVar 1)) (EMVar 2)
    , ruleRHS = EAp (EAp (EMVar 2) (EMVar 0)) (EMVar 1)
    , ruleProcess = NoShow Just
    }
  , Rule
    { ruleName = "Fst"
    , ruleLHS = EAp (EAtom (Comb Fst)) (EMVar 2)
    , ruleRHS = EAp (EMVar 2) (EAtom (Comb CTrue))
    , ruleProcess = NoShow Just
    }
  , Rule
    { ruleName = "Snd"
    , ruleLHS = EAp (EAtom (Comb Snd)) (EMVar 2)
    , ruleRHS = EAp (EMVar 2) (EAtom (Comb CFalse))
    , ruleProcess = NoShow Just
    }
  , Rule
    { ruleName = "Nil"
    , ruleLHS = EAp (EAtom (Comb Nil)) (EMVar 0)
    , ruleRHS = EAtom (Comb CTrue)
    , ruleProcess = NoShow Just
    }
  , Rule
    { ruleName = "IsNil/Nil"
    , ruleLHS = EAp (EAtom (Comb IsNil)) (EAtom (Comb Nil))
    , ruleRHS = EAtom (Comb CTrue)
    , ruleProcess = NoShow Just
    }
  , Rule
    { ruleName = "IsNil/Pair"
    , ruleLHS = EAp (EAtom (Comb IsNil)) (EAp (EAp (EAtom (Comb Pair)) (EMVar 0)) (EMVar 1))
    , ruleRHS = EAtom (Comb CFalse)
    , ruleProcess = NoShow Just
    }
  , Rule
    { ruleName = "Point"
    , ruleLHS = EAtom (Comb Point)
    , ruleRHS = EAtom (Comb Pair)
    , ruleProcess = NoShow Just
    }
  , Rule
    { ruleName = "MkChecker"
    , ruleLHS = EAtom (Comb MkChecker)
    , ruleRHS = EAp (EAp (EAtom (Comb S)) (EAp (EAp (EAtom (Comb Compose)) (EAtom (Comb S))) (EAp (EAp (EAtom (Comb Flip)) (EAp (EAp (EAtom (Comb Compose)) (EAtom (Comb Flip))) (EAp (EAp (EAtom (Comb Compose)) (EAp (EAtom (Comb Flip)) (EAp (EAtom (Comb Flip)) (EAp (EAp (EAtom (Comb S)) (EAp (EAp (EAtom (Comb Compose)) (EAtom (Comb S))) (EAp (EAp (EAtom (Comb Compose)) (EAp (EAtom (Comb Compose)) (EAp (EAp (EAtom (Comb S)) (EAtom (Comb Id))) (EAtom (Comb Id))))) (EAtom (Comb Lt))))) (EAtom (Comb EqBool)))))) (EAp (EAp (EAtom (Comb S)) (EAtom (Comb Mul))) (EAtom (Comb Id)))))) (EAtom (Comb Nil))))) (EAp (EAp (EAtom (Comb S)) (EAp (EAp (EAtom (Comb Compose)) (EAtom (Comb S))) (EAp (EAp (EAtom (Comb Compose)) (EAp (EAtom (Comb Compose)) (EAtom (Comb Pair)))) (EAp (EAp (EAtom (Comb S)) (EAp (EAp (EAtom (Comb Compose)) (EAtom (Comb S))) (EAp (EAp (EAtom (Comb Compose)) (EAp (EAtom (Comb Compose)) (EAtom (Comb Pair)))) (EAp (EAtom (Comb Flip)) (EAtom (Comb Div)))))) (EAp (EAtom (Comb Flip)) (EAp (EAp (EAtom (Comb S)) (EAp (EAp (EAtom (Comb Compose)) (EAtom (Comb Compose))) (EAp (EAp (EAtom (Comb Flip)) (EAp (EAp (EAtom (Comb Compose)) (EAtom (Comb Compose))) (EAtom (Comb Add)))) (EAtom (Comb Neg))))) (EAp (EAp (EAtom (Comb Compose)) (EAp (EAtom (Comb S)) (EAtom (Comb Mul)))) (EAtom (Comb Div))))))))) (EAp (EAp (EAtom (Comb Flip)) (EAp (EAp (EAtom (Comb Compose)) (EAtom (Comb Compose))) (EAtom (Comb MkChecker)))) (EAp (EAp (EAtom (Comb Flip)) (EAtom (Comb Add))) (EAtom (Num 2)))))
    , ruleProcess = NoShow Just
    }
  ]
  where
    getInteger :: Expr Atom v -> Maybe Integer
    getInteger (EAtom (Num j)) = pure j
    getInteger _ = empty

    getList :: Expr Atom v -> Maybe [Expr Atom v]
    getList (EAp (EAp (EAtom (Comb Pair)) x) xs) = (x:) <$> getList xs
    getList (EAtom (Comb Nil)) = pure []
    getList _ = empty

    getPair :: Expr Atom v -> Maybe (Expr Atom v, Expr Atom v)
    getPair (EAp (EAp (EAtom (Comb Pair)) x) y) = pure (x, y)
    getPair _ = empty

    unaryIntOp :: (Integer -> Integer) -> NoShow (M.Map Integer (Expr Atom v) -> Maybe (M.Map Integer (Expr Atom v)))
    unaryIntOp f = NoShow $ \m -> do
      n <- getInteger =<< M.lookup 0 m
      pure $ M.fromList [(1, EAtom $ Num $ f n)]

    binaryIntOp :: (Integer -> Integer -> Integer) -> NoShow (M.Map Integer (Expr Atom v) -> Maybe (M.Map Integer (Expr Atom v)))
    binaryIntOp f = NoShow $ \m -> do
      n <- getInteger =<< M.lookup 0 m
      m <- getInteger =<< M.lookup 1 m
      pure $ M.fromList [(2, EAtom $ Num $ f n m)]

makeRule :: Show v => (MetaExpr Atom Integer v, MetaExpr Atom Integer v) -> Rule Atom Integer v
makeRule (lhs, rhs) = Rule
  { ruleName = show lhs ++ " = " ++ show rhs
  , ruleLHS = lhs
  , ruleRHS = rhs
  , ruleProcess = NoShow Just
  }
