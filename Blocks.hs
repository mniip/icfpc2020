{-# LANGUAGE BangPatterns #-}
module Blocks where

import Data.Tuple
import Data.List
import Data.Bits
import Control.Arrow
import Control.Applicative
import Control.Monad
import Data.Ix
import Data.List.Split
import qualified Data.Set as S
import Numeric.Natural

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
    border = orthoConnectedComponent index (0, 0)

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
  = BNum Integer
  | BEq
  | BAp
  | BSucc
  | BPred
  | BAdd
  | BVar Integer
  | BMul
  | BDiv
  | BBEq -- ^ Boolean equality
  | BTrue
  | BFalse
  | BLt
  | BMod -- ^ Modulate
  | BDem -- ^ Demodulate
  | BEval -- ^ Unknown lambda looking symbol
  | BUnknownOp Int Int Natural
  | BUnknown Int Int Natural
  deriving (Eq, Ord, Show)

numValue :: [[Bool]] -> Natural
numValue = go 0 0 . concat
  where
    go !n !i [] = n
    go n i (False:xs) = go n (i + 1) xs
    go n i (True:xs) = go (setBit n i) (i + 1) xs

parseBlock :: [[Bool]] -> BlockType
parseBlock xs
  | width == height
  , and (tail $ head xs) && all head (tail xs) && not (head . head $ xs)
  = BNum $ toInteger $ numValue $ tail <$> tail xs
  | width + 1 == height
  , and (tail $ head xs) && all head (tail xs) && not (head . head $ xs) && not (and $ tail $ last xs)
  = BNum $ negate $ toInteger $ numValue $ tail <$> tail xs
  | width == height
  , and (head xs) && all head xs
  = case (width - 1, height - 1, numValue $ tail <$> tail xs) of
      (1, 1, 0   ) -> BAp
      (2, 2, 12  ) -> BEq
      (3, 3, 40  ) -> BDiv
      (3, 3, 146 ) -> BMul
      (3, 3, 170 ) -> BMod
      (3, 3, 174 ) -> BEval
      (3, 3, 341 ) -> BDem
      (3, 3, 365 ) -> BAdd
      (3, 3, 401 ) -> BPred
      (3, 3, 416 ) -> BLt
      (3, 3, 417 ) -> BSucc
      (3, 3, 448 ) -> BBEq
      (w, h, i   ) -> BUnknownOp w h i
  | and (head xs) && and (last xs) && all head xs && all last xs
  , BNum i <- parseBlock $ map (init . tail) . init . tail $ xs
  = BVar i
  | otherwise
  = BUnknown width height $ numValue xs
  where
    width = length $ head xs
    height = length xs

displayParsed :: [[Bool]] -> IO ()
displayParsed grid
  = forM_ [0..height-1] $ \y -> do
      putStrLn $ concatMap (\x -> str (x, y)) [0..width-1]
  where
    str (x, y) = case find (\b -> inRange (fst b) (x, y)) blocks of
      Just (r, b) -> (if grid !! y !! x then "\x1B[101m" else "") ++ [show' b !! index (swap *** swap $ r) (y, x)] ++ "\x1B[0m"
      Nothing     -> if grid !! y !! x then "█" else " "

    show' (BUnknownOp _ _ i) = ":" ++ show i ++ repeat ' '
    show' (BUnknown _ _ i) = "?" ++ show i ++ repeat ' '
    show' (BNum i) = show i ++ repeat ' '
    show' b = show b ++ repeat ' '

    blocks = map (\r -> (r, parseBlock $ selectBlock r grid)) $ findBlocksIgnoringBorder grid

    width = length $ head grid
    height = length grid
