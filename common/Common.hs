{-# LANGUAGE LambdaCase #-}
module Common where

import Data.Bits
import Data.List
import Control.Monad.Trans.State
import Control.Monad
import Control.Applicative
import Data.Maybe

data IntList
  = LInt !Integer
  | LCons !IntList !IntList
  | LNil
  deriving (Eq, Ord, Show, Read)

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
