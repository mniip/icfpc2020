{-# LANGUAGE LambdaCase, ViewPatterns #-}

module Eval where

import Numeric.Natural
import Control.Monad.Trans.State
import Control.Monad
import Control.Applicative
import Data.List
import Data.Char
import Data.Maybe
import Data.IORef
import Control.Exception
import System.IO (hFlush, stdout)

import Constants
import VM

data Token = TEq | TAp | TName Natural | TNum Integer | TVar Natural
  deriving (Eq, Ord, Show)

data Expr
  = ENum Integer
  | EGlob Natural
  | Expr :$ Expr
  | EVar Natural
  deriving (Eq, Ord, Show)
infixl 1 :$

tokenize :: String -> [Token]
tokenize = map mkToken . words
  where
    mkToken num | all isNum num = TNum $ read num
    mkToken ('x':num) | all isNum num = TVar $ read num
    mkToken (':':num) | all isNum num = mkName $ read num
    mkToken name = mkName $ fromMaybe (error $ "Unknown " ++ name) $ fromPegovkaOpNum name

    mkName num | num == apOpNum = TAp
               | num == eqOpNum = TEq
               | otherwise      = TName num

    isNum c = isDigit c || c == '-'

data ReplStmt
  = Decl Natural [Natural] Expr
  | Whnf Expr
  deriving (Eq, Ord, Show)

parseLine :: String -> ReplStmt
parseLine = fromMaybe (error "no parse") . evalStateT (pLine <* pEnd) . tokenize
  where
    pEnd = StateT $ \case
      [] -> pure ((), [])
      _  -> empty

    pLine = pDecl <|> (Whnf <$> pExpr)

    pDecl = do
      lhs <- pExpr
      let (name, bndr) = go [] lhs
      void $ exact TEq
      rhs <- pExpr
      pure $ Decl name bndr rhs
      where go vs (e :$ EVar v) = go (v:vs) e
            go vs (EGlob name) = (name, vs)
            go _ _ = error "expected a binder"

    pExpr = token >>= \case
      TAp       -> (:$) <$> pExpr <*> pExpr
      TNum num  -> pure $ ENum num
      TVar num  -> pure $ EVar num
      TName num -> pure $ EGlob num
      _         -> empty

    token = StateT uncons
    satisfies p = mfilter p token
    exact t = satisfies (== t)

compile :: [Natural] -> Expr -> IO Closure
compile bndrs e = newFun (length bndrs) =<< compileEntry e
  where
    compileEntry (EGlob i) = pure $ EntryGlobal i
    compileEntry (f :$ x) = EntryApply <$> compileEntry f <*> compileEntry x
    compileEntry (ENum i) = EntryValue <$> newInt i
    compileEntry (EVar v) = case elemIndex v bndrs of
      Just i -> pure $ EntryArg i
      Nothing -> error $ "Unbound variable " ++ show v

runStmt :: IORef Globals -> ReplStmt -> IO ()
runStmt globals (Decl name bndr expr) = do
  glob <- readIORef globals
  clos <- compile bndr expr
  glob' <- addGlobal glob name clos
  writeIORef globals glob'
runStmt globals (Whnf expr) = do
  glob <- readIORef globals
  clos <- compile [] expr
  whnfPpr glob clos
  putStrLn ""

run :: IORef Globals -> String -> IO ()
run globals str = catch (runStmt globals $ parseLine str) (\e -> print (e :: SomeException))

interaction :: IORef Globals -> Natural -> IO ()
interaction globals name = do
  glob <- readIORef globals
  zero <- newInt 0
  clos <- newThunk $ EntryGlobal interactOpNum `EntryApply` EntryGlobal name `EntryApply` EntryGlobal nilOpNum `EntryApply` (EntryGlobal pairOpNum `EntryApply` EntryValue zero `EntryApply` EntryValue zero)
  go glob clos
  where
    go glob clos = do
      (state:drawings:_) <- whnfList glob clos
      mapM_ (whnfUglyPrint glob) =<< whnfList glob drawings
      putStrLn "I"
      hFlush stdout
      [x, y] <- map read . words <$> getLine
      cx <- newInt x
      cy <- newInt y
      clos' <- newThunk $ EntryGlobal interactOpNum `EntryApply` EntryGlobal name `EntryApply` EntryValue state `EntryApply` (EntryGlobal pairOpNum `EntryApply` EntryValue cx `EntryApply` EntryValue cy)
      go glob clos'

runProgram :: [String] -> IO ()
runProgram strs = do
    globals <- (newIORef =<< mkGlobals)
    mapM_ (run globals) strs
