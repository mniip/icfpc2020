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
import System.IO.Unsafe

import Constants
import VM

data Token = TEq | TAp | TName Natural | TNum Integer
  deriving (Eq, Ord, Show)

data Expr
  = ENum Integer
  | EGlob Natural
  | Expr :$ Expr
  deriving (Eq, Ord, Show)
infixl 1 :$

tokenize :: String -> [Token]
tokenize = map mkToken . words
  where
    mkToken num | all isNum num = TNum $ read num
    mkToken (':':num) | all isNum num = mkName $ read num
    mkToken name = mkName $ fromMaybe (error $ "Unknown " ++ name) $ fromPegovkaOpNum name

    mkName num | num == apOpNum = TAp
               | num == eqOpNum = TEq
               | otherwise      = TName num

    isNum c = isDigit c || c == '-'

data ReplStmt
  = Decl Natural Expr
  | Whnf Expr
  deriving (Eq, Ord, Show)

parseLine :: String -> ReplStmt
parseLine = fromMaybe (error "no parse") . evalStateT (pLine <* pEnd) . tokenize
  where
    pEnd = StateT $ \case
      [] -> pure ((), [])
      _  -> empty

    pLine = pDecl <|> (Whnf <$> pExpr)

    pDecl = Decl <$> pName <* exact TEq <*> pExpr

    pName = token >>= \case
      TName n -> pure n
      _       -> empty

    pExpr = token >>= \case
      TAp       -> (:$) <$> pExpr <*> pExpr
      TNum num  -> pure $ ENum num
      TName num -> pure $ EGlob num
      _         -> empty

    token = StateT uncons
    satisfies p = mfilter p token
    exact t = satisfies (== t)

compile :: Expr -> IO Closure
compile (ENum i) = newInt i
compile e = newThunk =<< compileEntry e
  where
    compileEntry (EGlob i) = pure $ EntryGlobal i
    compileEntry (f :$ x) = EntryApply <$> compileEntry f <*> compileEntry x
    compileEntry expr = do
      EntryValue <$> compile expr

runStmt :: IORef Globals -> ReplStmt -> IO ()
runStmt globals (Decl num expr) = do
  glob <- readIORef globals
  clos <- compile expr
  glob' <- addGlobal glob num clos
  writeIORef globals glob'
runStmt globals (Whnf expr) = do
  glob <- readIORef globals
  clos <- compile expr
  whnfPpr glob clos
  putStrLn ""

run :: IORef Globals -> String -> IO ()
run globals str = catch (runStmt globals $ parseLine str) (\e -> print (e :: SomeException))

runProgram :: [String] -> IO ()
runProgram strs = do
    globals <- (newIORef =<< mkGlobals)
    mapM_ (run globals) strs
