{-# LANGUAGE LambdaCase #-}
module VM where

import Control.Monad
import Control.Exception
import Data.IORef
import Data.Array.Unboxed
import Numeric.Natural
import qualified Data.HashMap.Strict as HM
import Data.Unique

import Constants

newtype Closure = Closure (IORef ClosureData)

readClosure :: Closure -> IO ClosureData
readClosure (Closure ref) = readIORef ref

updateClosure :: Closure -> ClosureData -> IO ()
updateClosure (Closure ref) cd = writeIORef ref cd

newClosure :: ClosureData -> IO Closure
newClosure cd = Closure <$> newIORef cd

newInt :: Integer -> IO Closure
newInt i = newClosure $ ClosureInt i

newThunk :: EntryCode -> IO Closure
newThunk entry = newFun 0 entry

newFun :: Int -> EntryCode -> IO Closure
newFun arity entry = newClosure $ ClosureFun arity [] entry

data ClosureData
  = ClosureInt Integer
  | ClosureBits (UArray Int Bool)
  | ClosureImage (UArray (Int, Int) Bool)
  | ClosureFun Int [Closure] EntryCode
  -- ^ unsaturated arity, partially applied arguments
  | ClosureAlias Closure

data EntryCode
  = EntryGlobal Natural
  | EntryArg Int
  | EntryValue Closure
  | EntryApply EntryCode EntryCode
  | EntryBuiltin (Globals -> [Closure] -> Closure -> IO ())
infixl 1 `EntryApply`

newtype Globals = Globals { getGlobals :: HM.HashMap Natural Closure }

isWhnf :: ClosureData -> Bool
isWhnf (ClosureInt _) = True
isWhnf (ClosureBits _) = True
isWhnf (ClosureImage _) = True
isWhnf (ClosureFun n _ _) = n /= 0
isWhnf (ClosureAlias _) = False

whnf :: Globals -> Closure -> IO ()
whnf glob self = readClosure self >>= \case
  ClosureInt _ -> pure ()
  ClosureBits _ -> pure ()
  ClosureImage _ -> pure ()
  ClosureFun n args entry -> if n == 0
                             then enterUpdate glob args entry self
                             else pure ()
  ClosureAlias clos -> do
    whnf glob clos
    updateClosure self =<< readClosure clos

enterUpdate :: Globals -> [Closure] -> EntryCode -> Closure -> IO ()
enterUpdate glob _ (EntryGlobal i) self = do
  let clos = HM.findWithDefault (error $ "Missing global " ++ show i) i $ getGlobals glob
  updateClosure self $ ClosureAlias clos
  whnf glob self
enterUpdate glob args (EntryArg i) self = do
  let clos = reverse args !! i
  updateClosure self $ ClosureAlias clos
  whnf glob self
enterUpdate glob _ (EntryValue clos) self = do
  updateClosure self $ ClosureAlias clos
  whnf glob self
enterUpdate glob args (EntryApply fun arg) self = do
  funclos <- newClosure $ ClosureFun 0 args fun
  argclos <- newClosure $ ClosureFun 0 args arg
  whnf glob funclos
  readClosure funclos >>= \case
    ClosureFun funarity funargs entry | funarity > 0 -> do
      updateClosure self $ ClosureFun (funarity - 1) (argclos:funargs) entry
      whnf glob self
    cd -> error $ "Expected function, got " ++ pprClosureData cd
enterUpdate glob args (EntryBuiltin f) self = f glob args self

pprClosureData :: ClosureData -> String
pprClosureData (ClosureInt i) = show i
pprClosureData (ClosureBits bits) = "[" ++ map (\b -> if b then '0' else '1') (elems bits) ++ "]"
pprClosureData (ClosureImage image) = "Image (" ++ show w ++ "x" ++ show h ++ ")"
  where (w, h) = bounds image
pprClosureData (ClosureFun n _ entry) = (if n == 0 then "Thunk" else "Fun/" ++ show n) ++ " " ++ pprEntry entry
  where
    pprEntry (EntryGlobal i) = ":" ++ show i
    pprEntry (EntryArg i) = "$" ++ show i
    pprEntry (EntryValue _) = "_"
    pprEntry (EntryApply f x) = "(" ++ pprEntry f ++ " " ++ pprEntry x ++ ")"
    pprEntry (EntryBuiltin _) = "builtin"
pprClosureData (ClosureAlias _) = "Alias"

whnfInteger :: Globals -> Closure -> IO Integer
whnfInteger glob clos = do
  whnf glob clos
  readClosure clos >>= \case
    ClosureInt i -> pure i
    cd           -> error $ "Expected int, got " ++ pprClosureData cd

whnfBool :: Globals -> Closure -> IO Bool
whnfBool glob clos = do
  no <- toInteger . hashUnique <$> newUnique
  let yes = no + 1
  noClos <- newInt no
  yesClos <- newInt yes
  targ <- newThunk $ EntryValue clos `EntryApply` EntryValue yesClos `EntryApply` EntryValue noClos
  whnfInteger glob targ >>= \case
    i | i == no   -> pure False
      | i == yes  -> pure True
      | otherwise -> error $ "Expected a boolean, mapped " ++ show (no, yes) ++ " to " ++ show i

mkIntUnaryOp :: (Integer -> Integer) -> IO Closure
mkIntUnaryOp f = newFun 1 $ EntryBuiltin builtin
  where
    builtin glob [x] self = do
      i <- whnfInteger glob x
      r <- evaluate $ f i
      updateClosure self $ ClosureInt r
    builtin _ args _ = error $ "Expected 1 argument, got " ++ show (length args)

mkIntBinaryOp :: (Integer -> Integer -> Integer) -> IO Closure
mkIntBinaryOp f = newFun 2 $ EntryBuiltin builtin
  where
    builtin glob [y, x] self = do
      i <- whnfInteger glob x
      j <- whnfInteger glob y
      r <- evaluate $ f i j
      updateClosure self $ ClosureInt r
    builtin _ args _ = error $ "Expected 2 arguments, got " ++ show (length args)

mkIntUnaryPred ::  (Integer -> Bool) -> IO Closure
mkIntUnaryPred f = newFun 1 $ EntryBuiltin builtin
  where
    builtin glob [x] self = do
      i <- whnfInteger glob x
      if f i
      then updateClosure self $ ClosureFun 2 [] $ EntryArg 0
      else updateClosure self $ ClosureFun 2 [] $ EntryArg 1
    builtin _ args _ = error $ "Expected 1 argument, got " ++ show (length args)

mkIntBinaryPred :: (Integer -> Integer -> Bool) -> IO Closure
mkIntBinaryPred f = newFun 2 $ EntryBuiltin builtin
  where
    builtin glob [y, x] self = do
      i <- whnfInteger glob x
      j <- whnfInteger glob y
      if f i j
      then updateClosure self $ ClosureFun 2 [] $ EntryArg 0
      else updateClosure self $ ClosureFun 2 [] $ EntryArg 1
    builtin _ args _ = error $ "Expected 2 arguments, got " ++ show (length args)

whnfList :: Globals -> Closure -> IO [Closure]
whnfList glob clos = do
  targ <- newThunk $ EntryGlobal isNilOpNum `EntryApply` EntryValue clos
  whnfBool glob targ >>= \case
    True  -> pure []
    False -> do
      fstClos <- newThunk $ EntryGlobal fstOpNum `EntryApply` EntryValue clos
      sndClos <- newThunk $ EntryGlobal sndOpNum `EntryApply` EntryValue clos
      (fstClos:) <$> whnfList glob sndClos

data IntList
  = LInt Integer
  | LCons IntList IntList
  | LNil
  deriving (Eq, Ord, Show)

newIntList :: IntList -> IO Closure
newIntList (LInt i) = newInt i
newIntList (LCons x xs) = do
  c <- newIntList x
  cs <- newIntList xs
  newThunk $ EntryGlobal pairOpNum `EntryApply` EntryValue c `EntryApply` EntryValue cs
newIntList LNil = newThunk $ EntryGlobal nilOpNum

whnfIntList :: Globals -> Closure -> IO IntList
whnfIntList glob clos = do
  whnf glob clos
  readClosure clos >>= \case
    ClosureInt i -> pure $ LInt i
    _            -> do
      targ <- newThunk $ EntryGlobal isNilOpNum `EntryApply` EntryValue clos
      whnfBool glob targ >>= \case
        True  -> pure LNil
        False -> do
          fstClos <- newThunk $ EntryGlobal fstOpNum `EntryApply` EntryValue clos
          sndClos <- newThunk $ EntryGlobal sndOpNum `EntryApply` EntryValue clos
          LCons <$> whnfIntList glob fstClos <*> whnfIntList glob sndClos

whnfPpr :: Globals -> Closure -> IO ()
whnfPpr glob clos = do
  whnf glob clos
  readClosure clos >>= \case
    ClosureInt i -> putStr $ show i
    ClosureBits bits -> putStr $ "[" ++ map (\b -> if b then '0' else '1') (elems bits) ++ "]"
    ClosureImage image | let (_, (w, h)) = bounds image
      -> do putStrLn ""
            forM_ [0..w] $ \y -> do
              forM_ [0..h] $ \x ->
                putStr (if image ! (x, y) then "█" else " ")
              putStrLn ""
    cd -> do
      targ <- newThunk $ EntryGlobal isNilOpNum `EntryApply` EntryValue clos
      whnfBool' glob targ >>= \case
        Nothing    -> putStr "?{" >> putStr (pprClosureData cd) >> putStr "}"
        Just True  -> putStr "nil"
        Just False -> do
          fstClos <- newThunk $ EntryGlobal fstOpNum `EntryApply` EntryValue clos
          sndClos <- newThunk $ EntryGlobal sndOpNum `EntryApply` EntryValue clos
          putStr "(cons "
          whnfPpr glob fstClos
          putStr " "
          whnfPpr glob sndClos
          putStr ")"
      where
        whnfBool' glob clos = do
          no <- toInteger . hashUnique <$> newUnique
          let yes = no + 1
          noClos <- newInt no
          yesClos <- newInt yes
          targ <- newThunk $ EntryValue clos `EntryApply` EntryValue yesClos `EntryApply` EntryValue noClos
          whnf glob targ
          readClosure targ >>= \case
            ClosureInt i | i == no   -> pure $ Just False
                         | i == yes  -> pure $ Just True
            _                        -> pure $ Nothing

mkGlobals :: IO Globals
mkGlobals = do
  idClos <- newFun 1 $ EntryArg 0
  trueClos <- newFun 2 $ EntryArg 0
  compClos <- newFun 3 $ EntryArg 0 `EntryApply` (EntryArg 1 `EntryApply` EntryArg 2)
  flipClos <- newFun 3 $ EntryArg 0 `EntryApply` EntryArg 2 `EntryApply` EntryArg 1
  sClos <- newFun 3 $ EntryArg 0 `EntryApply` EntryArg 2 `EntryApply` (EntryArg 1 `EntryApply` EntryArg 2)
  falseClos <- newFun 2 $ EntryArg 1
  negClos <- mkIntUnaryOp negate
  nilClos <- newFun 1 $ EntryGlobal trueOpNum
  isNilClos <- newFun 1 $ EntryArg 0 `EntryApply` (EntryGlobal trueOpNum `EntryApply` (EntryGlobal trueOpNum `EntryApply` EntryGlobal falseOpNum))
  divClos <- mkIntBinaryOp quot
  mulClos <- mkIntBinaryOp (*)
  modClos <- newFun 1 $ EntryBuiltin builtinMod
  evalClos <- newFun 1 $ EntryBuiltin builtinEval
  demClos <- newFun 1 $ EntryBuiltin builtinDem
  addClos <- mkIntBinaryOp (+)
  predClos <- mkIntUnaryOp pred
  ltClos <- mkIntBinaryPred (<)
  succClos <- mkIntUnaryOp succ
  bEqClos <- mkIntBinaryPred (==)
  chooseClos <- mkIntUnaryPred $ \case 0 -> True; 1 -> False; i -> error $ "Choose " ++ show i
  pairClos <- newFun 3 $ EntryArg 2 `EntryApply` EntryArg 0 `EntryApply` EntryArg 1
  fstClos <- newFun 1 $ EntryArg 0 `EntryApply` EntryGlobal trueOpNum
  sndClos <- newFun 1 $ EntryArg 0 `EntryApply` EntryGlobal falseOpNum
  let pointClos = pairClos
  imageClos <- newFun 1 $ EntryBuiltin builtinImage
  mapImageOpNum <- newFun 1 $ EntryGlobal isNilOpNum `EntryApply` EntryArg 0 `EntryApply` EntryGlobal nilOpNum `EntryApply` (EntryGlobal pairOpNum `EntryApply` (EntryGlobal imageOpNum `EntryApply` (EntryGlobal fstOpNum `EntryApply` EntryArg 0)) `EntryApply` (EntryGlobal mapImageOpNum `EntryApply` (EntryGlobal sndOpNum `EntryApply` EntryArg 0)))
  pure $ Globals $ HM.fromList
    [ (idOpNum, idClos)
    , (trueOpNum, trueClos)
    , (compOpNum, compClos)
    , (flipOpNum, flipClos)
    , (sOpNum, sClos)
    , (falseOpNum, falseClos)
    , (negOpNum, negClos)
    , (nilOpNum, nilClos)
    , (isNilOpNum, isNilClos)
    , (divOpNum, divClos)
    , (mulOpNum, mulClos)
    , (modOpNum, modClos)
    , (evalOpNum, evalClos)
    , (addOpNum, addClos)
    , (predOpNum, predClos)
    , (ltOpNum, ltClos)
    , (succOpNum, succClos)
    , (bEqOpNum, bEqClos)
    , (chooseOpNum, chooseClos)
    , (pairOpNum, pairClos)
    , (fstOpNum, fstClos)
    , (sndOpNum, sndClos)
    , (pointOpNum, pointClos)
    ]
  where
    builtinMod glob [x] self = undefined
    builtinMod _ args _ = error $ "Expected 1 argument, got " ++ show (length args)
    builtinEval glob [x] self = undefined
    builtinEval _ args _ = error $ "Expected 1 argument, got " ++ show (length args)
    builtinDem glob [x] self = undefined
    builtinDem _ args _ = error $ "Expected 1 argument, got " ++ show (length args)
    builtinImage glob [x] self = undefined
    builtinImage _ args _ = error $ "Expected 1 argument, got " ++ show (length args)

addGlobal :: Globals -> Natural -> Closure -> IO Globals
addGlobal glob num clos = pure $ Globals $ HM.insert num clos $ getGlobals glob
