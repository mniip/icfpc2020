{-# LANGUAGE MagicHash, UnboxedTuples, LambdaCase #-}
module Alien.FFI where

import GHC.Prim
import GHC.Word
import Unsafe.Coerce

import Data.Bits
import Data.List
import Control.Monad.Trans.State
import Control.Monad
import Control.Applicative
import Data.Maybe

import qualified Alien.Prelude as A

-- Really more like, after WHNF, is this a closure of a constructor of some datatype, or is it still a function?
isData :: alienValue -> Bool
isData x = x `seq` case unpackClosure# x of
  (# infoTable, _, _ #)
    | let closType = W# (indexWord32OffAddr# infoTable 2#)
    -> closType > 0 && closType < 8 {- ClosureTypes.h: 1-7 are CONSTR_* -}

data IntList
  = LInt !Integer
  | LCons !IntList !IntList
  | LNil
  deriving (Eq, Ord, Show)

extractIntList :: alienValue -> IntList
extractIntList x = if isData x
  then LInt $ unsafeCoerce x -- hopefully an Integer
  else unsafeCoerce A.isnil x LNil $ LCons (extractIntList $ unsafeCoerce A.car x) (extractIntList $ unsafeCoerce A.cdr x)

injectIntList :: IntList -> alienValue
injectIntList LNil = unsafeCoerce A.nil
injectIntList (LCons car cdr) = unsafeCoerce A.cons (injectIntList car) (injectIntList cdr)
injectIntList (LInt int) = unsafeCoerce int

modulate :: IntList -> [Bool]
modulate (LInt n)     = (if n >= 0 then [False, True] else [True, False])
                        ++ replicate len True
                        ++ [False]
                        ++ map (testBit absn) (reverse [0 .. 4*len-1])
                        where
                          absn = abs n
                          len = head $ dropWhile (\l -> 16^l <= absn) [0..]
modulate LNil         = [False, False]
modulate (LCons x xs) = [True, True] ++ modulate x ++ modulate xs

demodulate :: [Bool] -> IntList
demodulate = fromMaybe (error "Demodulate parse error") . evalStateT (go <* end)
  where
    bit = StateT uncons
    end = StateT $ \case
      [] -> pure ((), [])
      _  -> empty
    exact t = mfilter (== t) bit
    go = liftA2 (,) bit bit >>= \case
      (False, False) -> pure LNil
      (True, True) -> LCons <$> go <*> go
      (sign, _) -> LInt <$> do
        let getLen = bit >>= \b -> if b then succ <$> getLen else pure 0
        len <- getLen
        mantissa <- replicateM (4 * len) bit
        pure $ (if sign then negate else id) $ foldl' (\x y -> 2*x + if y then 1 else 0) 0 mantissa

newtype Drawing = Drawing [(Integer, Integer)] deriving (Eq, Ord, Show)
newtype AlienState = AlienState IntList deriving (Eq, Ord, Show)

-- Left - want HTTP request, Right - want click
interactWith :: alienValue -> AlienState -> IntList -> (AlienState, Either IntList [Drawing])
interactWith interactor (AlienState state) input =
  let response = unsafeCoerce interactor (injectIntList state) (injectIntList input)
    in case extractIntList (A.car response) of
         LInt 0 -> (AlienState . extractIntList $ A.car . A.cdr $ response, Right $ toPictures $ extractIntList $ A.car . A.cdr . A.cdr $ response )
         LInt 1 -> (AlienState . extractIntList $ A.car . A.cdr $ response, Left $ extractIntList $ A.car . A.cdr . A.cdr $ response)
         xs     -> error $ "interactor should return (cons 0/1 ...), got: " ++ show xs
  where
    toPictures LNil = []
    toPictures (LCons x xs) = Drawing (toPicture x) : toPictures xs
    toPictures l = error $ "toPictures " ++ show l
    toPicture LNil = []
    toPicture (LCons x xs) = toPoint x : toPicture xs
    toPicture l = error $ "toPicture " ++ show l
    toPoint (LCons (LInt x) (LInt y)) = (x, y)
    toPoint l = error $ "toPoint " ++ show l

-- Execute until next click is prompted
makeClick
  :: Monad m
  => (IntList -> m IntList) -- ^ run HTTP request
  -> (AlienState -> m ()) -- ^ executed on every change of state
  -> alienValue
  -> AlienState
  -> (Integer, Integer)
  -> m (AlienState, [Drawing])
makeClick send step interactor state (x, y) = go state (LCons (LInt x) (LInt y))
  where
    go state input = step state >> case interactWith interactor state input of
      (state', Left request) -> go state' =<< send request
      (state', Right pictures) -> pure (state', pictures)

runInteraction
  :: Monad m
  => (IntList -> m IntList) -- ^ run HTTP request
  -> ([Drawing] -> m (Integer, Integer)) -- ^ prompt for click
  -> (AlienState -> m ()) -- ^ executed on every change of state
  -> alienValue
  -> AlienState -- ^ initial state
  -> m ()
runInteraction send click step interactor initState = click [] >>= goClick initState
  where
    goClick state (x, y) = go state (LCons (LInt x) (LInt y))
    go state input = step state >> case interactWith interactor state input of
      (state', Left request) -> send request >>= go state'
      (state', Right pictures) -> click pictures >>= goClick state'
