module Calculus where

import Control.Applicative
import Control.Monad.Trans.State
import Data.Void
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S

-- | An expression in a lambda calculus with quantifiable variables 'v', metavariables 'm', and atoms/combinators 'a'
data MetaExpr a m v
  = EAtom a
  | EVar v
  | EMVar m
  | EAp (MetaExpr a m v) (MetaExpr a m v)
  | ELam v (MetaExpr a m v)
  deriving (Eq, Show)
infixl 1 `EAp`

type Expr a v = MetaExpr a Void v

freeVars :: Ord v => MetaExpr a m v -> S.Set v
freeVars (EAtom _) = S.empty
freeVars (EVar v) = S.singleton v
freeVars (EMVar _) = S.empty
freeVars (EAp f x) = S.union (freeVars f) (freeVars x)
freeVars (ELam v e) = S.delete v (freeVars e)

-- | Substitute variable(s). Avoid captures
substF :: Ord v => (Either m v -> MetaExpr a n v) -> MetaExpr a m v -> Maybe (MetaExpr a n v)
substF h = go S.empty
  where
    go _ (EAtom a) = pure $ EAtom a
    go c (EVar v) | v `S.member` c
                  = pure $ EVar v
                  | freeVars e `S.disjoint` c
                  = pure e
                  | otherwise
                  = empty
                  where e = h $ Right v
    go _ (EMVar m) = pure $ h $ Left m
    go c (EAp f x) = EAp <$> go c f <*> go c x
    go c (ELam v e) = ELam v <$> go (S.insert v c) e

newtype NoShow a = NoShow { unNoShow :: a }
instance Show (NoShow a) where show _ = "_"

-- | A rewriting rule. Metavariables are "bound" in the LHS and "used" in the RHS
data Rule a m v = Rule
  { ruleName    :: String
  , ruleLHS     :: MetaExpr a m v
  , ruleRHS     :: MetaExpr a m v
  , ruleProcess :: NoShow (M.Map m (Expr a v) -> Maybe (M.Map m (Expr a v)))
  } deriving Show

match :: (Eq a, Ord m, Eq v) => MetaExpr a m v -> Expr a v -> Maybe (M.Map m (Expr a v))
match metaexpr expr = execStateT (go metaexpr expr) M.empty
  where
    go (EMVar m) e = modify (M.insert m e)
    go _ (EMVar m) = absurd m
    go (EAtom a1) (EAtom a2) | a1 == a2 = pure ()
    go (EVar v1) (EVar v2) | v1 == v2 = pure ()
    go (EAp f1 x1) (EAp f2 x2) = go f1 f2 >> go x1 x2
    go (ELam _ _) _ = error "ELam in Rule LHS not supported"
    go _ _ = empty

substMap :: (Ord m, Ord v) => M.Map m (Expr a v) -> MetaExpr a m v -> Expr a v
substMap s = fromMaybe (error "Cannot capture by only substituting metavariables") . substF go
  where go (Left m) = fromMaybe (error "Unbound metavariable") $ M.lookup m s
        go (Right v) = EVar v

tryRules :: (Eq a, Ord m, Ord v) => [Rule a m v] -> Expr a v -> Maybe (Expr a v)
tryRules rules e = go rules
  where
    go [] = Nothing
    go (r:rs) | Just s <- match (ruleLHS r) e
              , Just s' <- unNoShow (ruleProcess r) s
              = Just $ substMap s' (ruleRHS r)
              | otherwise
              = go rs

smallStep :: (Eq a, Ord m, Ord v) => [Rule a m v] -> Expr a v -> Maybe (Expr a v)
smallStep rs = go
  where
    go e | Just e' <- tryRules rs e = Just e'
    go (EAp (ELam v e) x) | Just e' <- substF var e = pure e'
      where var (Right v') = if v == v' then x else EVar v'
            var (Left m) = absurd m
    go (EAp f x) = ((`EAp` x) <$> go f) <|> ((f `EAp`) <$> go x)
    go (ELam v e) = ELam v <$> go e
    go _ = empty
