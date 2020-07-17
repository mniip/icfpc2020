module Language where

import Numeric.Natural

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
