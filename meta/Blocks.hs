{-# LANGUAGE BangPatterns #-}
module Blocks where

import Control.Monad.Trans.State
import Data.Tuple
import Data.List
import Data.Bits
import Control.Arrow
import Control.Applicative
import Control.Monad
import Data.Function (on)
import Data.Ix
import Data.List.Split
import qualified Data.Set as S
import Numeric.Natural

import Language

wrap :: [Bool] -> [[Bool]]
wrap = chunksOf =<< guessWrap

guessWrap :: [Bool] -> Int
guessWrap xs = head $ dropWhile (not . hasBorder . (`chunksOf` xs)) [2..]
  where hasBorder xss = and (head xss) && and (last xss) && all head xss && all last xss

type Point i = (i, i)

-- | A range, assumed inclusive (to play with 'Ix' nicely)
type Block i = (Point i, Point i)

findBlock
  :: Enum i
  => (Point i -> Bool) -- ^ Data array
  -> Point i -- ^ Location of a 'True'
  -> Block i
findBlock grid (x, y) = go x y x y
  where
    go x1 y1 x2 y2
      | any (\y -> grid (x1, y) && any (\y -> grid (pred x1, y)) [pred y..succ y]) [y1..y2] -- left
        = go (pred x1) y1 x2 y2
      | any (\y -> grid (x2, y) && any (\y -> grid (succ x2, y)) [pred y..succ y]) [y1..y2] -- right
        = go x1 y1 (succ x2) y2
      | any (\x -> grid (x, y1) && any (\x -> grid (x, pred y1)) [pred x..succ x]) [x1..x2] -- top
        = go x1 (pred y1) x2 y2
      | any (\x -> grid (x, y2) && any (\x -> grid (x, succ y2)) [pred x..succ x]) [x1..x2] -- bottom
        = go x1 y1 x2 (succ y2)
      | otherwise
        = ((x1, y1), (x2, y2))

connectedComponent
  :: Ord p
  => (p -> [p]) -- ^ Neighbors
  -> (p -> Bool) -- ^ Data array
  -> p -- ^ Source of search
  -> S.Set p
connectedComponent neighbors grid p = go S.empty [p]
  where
    go seen [] = seen
    go seen (p:ps)
      | p `S.member` seen = go seen ps
      | grid p = go (S.insert p seen) $ neighbors p ++ ps
      | otherwise = go seen ps

orthoConnectedComponent, diagConnectedComponent
  :: (Enum i, Ord i)
  => (Point i -> Bool)
  -> Point i
  -> S.Set (Point i)
orthoConnectedComponent = connectedComponent
  $ \p -> fmap ($ p) [pred *** id, succ *** id, id *** pred, id *** succ]
diagConnectedComponent = connectedComponent
  $ \p -> fmap ($ p) [pred *** pred, pred *** id, pred *** succ, id *** pred, id *** succ, succ *** pred, succ *** id, succ *** succ]

findBlocksIn :: [Point Int] -> [[Bool]] -> [Block Int]
findBlocksIn rng grid = go S.empty rng
  where
    go seen (p:ps)
      | not $ unsafeIndex p = go seen ps
      | p `S.member` seen = go seen ps
      | r <- findBlock index p
      , nonTrivialBlock r
      = r : go (S.union seen $ S.fromList $ filter unsafeIndex $ range r) ps
      | otherwise
      = go (S.insert p seen) ps
    go seen [] = []

    nonTrivialBlock ((x1, y1), (x2, y2)) = x1 < x2 && y1 < y2

    width = length $ head grid
    height = length grid
    unsafeIndex (x, y) = grid !! y !! x
    index (x, y) = x >= 0 && x < width && y >= 0 && y < height && unsafeIndex (x, y)

findBlocks :: [[Bool]] -> [Block Int]
findBlocks grid = findBlocksIn (range ((0, 0), (width-1, height-1))) grid
  where
    width = length $ head grid
    height = length grid

findBlocksIgnoringBorder :: [[Bool]] -> [Block Int]
findBlocksIgnoringBorder grid
  = findBlocksIn (filter (`S.notMember` border) $ range ((0, 0), (width-1, height-1))) grid
  where
    border = diagConnectedComponent index (0, 0)

    width = length $ head grid
    height = length grid
    unsafeIndex (x, y) = grid !! y !! x
    index (x, y) = x >= 0 && x < width && y >= 0 && y < height && unsafeIndex (x, y)

display :: [[Bool]] -> IO ()
display = mapM_ $ putStrLn . map letter
  where
    letter True  = '█'
    letter False = ' '

displayBlocks :: [[Bool]] -> [Block Int] -> IO ()
displayBlocks grid blocks
  = forM_ [0..height-1] $ \y -> do
      putStrLn $ map (\x -> letter (x, y)) [0..width-1]
  where
    letter (x, y) = case (grid !! y !! x, any (`inRange` (x, y)) blocks) of
      (False, False) -> ' '
      (False, True)  -> '░'
      (True, False)  -> '█'
      (True, True)   -> '▓'

    width = length $ head grid
    height = length grid

selectBlock :: Block Int -> [[a]] -> [[a]]
selectBlock ((x1, y1), (x2, y2)) = map (drop x1 . take (x2 + 1)) . drop y1 . take (y2 + 1)

data BlockType
  = BAtom Atom
  | BEq
  | BAp
  | BVar Integer
  | BListOpen
  | BListComma
  | BListClose
  | BUnknownOp Int Int Natural
  | BUnknown Int Int Natural
  deriving (Eq, Ord, Show)

numValue :: [[Bool]] -> Natural
numValue = go 0 0 . concat
  where
    go !n !i [] = n
    go n i (False:xs) = go n (i + 1) xs
    go n i (True:xs) = go (setBit n i) (i + 1) xs

lineDecode :: [Bool] -> Maybe Integer
lineDecode xs = case runStateT go xs of
  Just (r, []) -> pure r
  _            -> empty
  where
    go = do sign <- bit
            expect (not sign)
            let getLen = bit >>= \b -> if b then succ <$> getLen else pure 0
            len <- getLen
            ((if sign then -1 else 1) *)
              <$> foldl' (\x y -> 2*x + if y then 1 else 0) 0
              <$> replicateM (4*len) bit
    bit = StateT uncons
    expect b = bit >>= \x -> if x == b then pure () else StateT mempty

intToLine :: Integer -> [Bool]                                                       
intToLine n = (if n >= 0 then [False, True] else [True, False])                      
  ++ replicate len True                                                         
  ++ [False]                                                                    
  ++ map (testBit absn) (reverse [0 .. 4*len-1])                                
  where                                                                         
    absn = abs n                                                                
    len = head $ dropWhile (\l -> 16^l <= absn) [0..]

-- Cons list
data NumList = NilL | NumL Integer | Cons NumList NumList deriving (Show)

listToLine :: NumList -> [Bool]
listToLine NilL = [False, False]
listToLine (NumL n) = intToLine n
listToLine (Cons a b) = [True, True] ++ listToLine a ++ listToLine b

lineToList :: [Bool] -> Maybe NumList
lineToList xs = case runStateT listState xs of
  Just (r, []) -> pure r
  _            -> empty

listState :: StateT [Bool] Maybe NumList
listState = do
    b1 <- bit
    b2 <- bit
    case (b1, b2) of
      (False, False) -> return NilL
      (True, True) -> do
          left <- listState
          right <- listState
          return (Cons left right)
      _ -> do
          num <- nm b1
          return (NumL num)
  where
    nm :: Bool -> StateT [Bool] Maybe Integer
    nm sign =
        do let getLen = bit >>= \b -> if b then succ <$> getLen else pure 0
           len <- getLen
           ((if sign then -1 else 1) *)
             <$> foldl' (\x y -> 2*x + if y then 1 else 0) 0
             <$> replicateM (4*len) bit
    bit = StateT uncons
    expect b = bit >>= \x -> if x == b then pure () else StateT mempty

parseBlock :: [[Bool]] -> BlockType
parseBlock xs
  | width == height
  , and (tail $ head xs) && all head (tail xs) && not (head . head $ xs)
  = BAtom $ Num $ toInteger $ numValue $ tail <$> tail xs
  | width + 1 == height
  , and (tail $ head xs) && all head (tail xs) && not (head . head $ xs) && not (and $ tail $ last xs)
  = BAtom $ Num $ negate $ toInteger $ numValue $ tail <$> tail xs
  | width >= 4 && height >= 4
  , and (head xs) && and (last xs) && all head xs && all last xs
  , BAtom (Num i) <- parseBlock $ map (map not . init . tail) . init . tail $ xs
  = BVar i
  | height == 2
  , xs !! 0 == map not (xs !! 1)
  = BAtom $ Bits $ xs !! 0
  | width == height
  , and (head xs) && all head xs
  = case (width - 1, height - 1, numValue $ tail <$> tail xs) of
      (1, 1, 0   ) -> BAp 
      (1, 1, 1   ) -> BAtom $ Comb Id
      (2, 2, 2   ) -> BAtom $ Comb CTrue
      (2, 2, 5   ) -> BAtom $ Comb Compose
      (2, 2, 6   ) -> BAtom $ Comb Flip
      (2, 2, 7   ) -> BAtom $ Comb S
      (2, 2, 8   ) -> BAtom $ Comb CFalse
      (2, 2, 10  ) -> BAtom $ Comb Neg
      (2, 2, 12  ) -> BEq
      (2, 2, 14  ) -> BAtom $ Comb Nil
      (2, 2, 15  ) -> BAtom $ Comb IsNil
      (3, 3, 40  ) -> BAtom $ Comb Div
      (3, 3, 146 ) -> BAtom $ Comb Mul
      (3, 3, 170 ) -> BAtom $ Comb Mod
      (3, 3, 174 ) -> BAtom $ Comb Eval
      (3, 3, 341 ) -> BAtom $ Comb Dem
      (3, 3, 365 ) -> BAtom $ Comb Add
      (3, 3, 401 ) -> BAtom $ Comb Pred
      (3, 3, 416 ) -> BAtom $ Comb Lt
      (3, 3, 417 ) -> BAtom $ Comb Succ
      (3, 3, 448 ) -> BAtom $ Comb EqBool
      (4, 4, 58336) -> BAtom $ Comb Choose
      (4, 4, 64170) -> BAtom $ Comb Pair
      (4, 4, 64174) -> BAtom $ Comb Fst
      (4, 4, 64171) -> BAtom $ Comb Snd
      (5, 5, 17043521) -> BAtom $ Comb Point
      (5, 5, 33047056) -> BAtom $ Comb MkImage
      (5, 5, 11184810) -> BAtom $ Comb MkChecker
      (6, 6, 68191693600) -> BAtom $ Comb Pow2
      (6, 6, 68259412260) -> BAtom $ Comb MapImage
      (w, h, i   ) -> BUnknownOp w h i
  | otherwise
  = case (width, height, numValue xs) of
      (3, 5, 19956) -> BListOpen
      (2, 5, 1023) -> BListComma
      (3, 5, 6105) -> BListClose
      (w, h, i   ) -> BUnknown w h i
  where
    width = length $ head xs
    height = length xs

displayParsed :: [[Bool]] -> IO ()
displayParsed grid
  = forM_ [0..height-1] $ \y -> do
      putStrLn $ concatMap (\x -> str (x, y)) [0..width-1]
  where
    str (x, y) = case find (\b -> inRange (fst b) (x, y)) blocks of
      Just (r, b) -> (if grid !! y !! x then "\x1B[101m" else "") ++ [(show' b ++ repeat '░') !! index (swap *** swap $ r) (y, x)] ++ "\x1B[0m"
      Nothing     -> if grid !! y !! x then "█" else " "

    show' (BUnknownOp _ _ i) = ":" ++ show i
    show' (BUnknown _ _ i) = "?" ++ show i
    {-
    show' (BNum i) = show i
    show' (BLineNum i) = "[" ++ show i ++ "]"
    -}
    show' b = tail $ show b

    blocks = map (\r -> (r, parseBlock $ selectBlock r grid)) $ findBlocksIgnoringBorder grid

    width = length $ head grid
    height = length grid

picToText :: [[Bool]] -> [[BlockType]]
picToText grid = map (map snd) blockLines
  where
    blockLines = groupBy ((==) `on` fst) . sortBy (compare `on` fst) $ blocks
    blocks = map (\r -> (snd $ fst r, parseBlock $ selectBlock r grid)) $ findBlocksIgnoringBorder grid

    width = length $ head grid
    height = length grid
