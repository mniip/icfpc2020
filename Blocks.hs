module Blocks where

import Control.Arrow
import Control.Applicative
import Control.Monad
import Data.Ix
import Data.List.Split
import qualified Data.Set as S

wrap :: [Bool] -> [[Bool]]
wrap = chunksOf =<< guessWrap

guessWrap :: [Bool] -> Int
guessWrap xs = head $ dropWhile (not . hasBorder . (`chunksOf` xs)) [2..]
  where hasBorder xss = and (head xss) && and (last xss) && all head xss && all last xss

findBlock
  :: (Enum i, Enum j)
  => ((i, j) -> Bool) -- ^ Data array
  -> (i, j) -- ^ Location of a 'True'
  -> ((i, j), (i, j)) -- ^ Inclusive range for the block
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
  :: (Enum i, Enum j, Ord i, Ord j)
  => ((i, j) -> Bool)
  -> (i, j)
  -> S.Set (i, j)
orthoConnectedComponent = connectedComponent
  $ \p -> fmap ($ p) [pred *** id, succ *** id, id *** pred, id *** succ]
diagConnectedComponent = connectedComponent
  $ \p -> fmap ($ p) [pred *** pred, pred *** id, pred *** succ, id *** pred, id *** succ, succ *** pred, succ *** id, succ *** succ]

findBlocksIn :: [(Int, Int)] -> [[Bool]] -> [((Int, Int), (Int, Int))]
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

findBlocks :: [[Bool]] -> [((Int, Int), (Int, Int))]
findBlocks grid = findBlocksIn (range ((0, 0), (width-1, height-1))) grid
  where
    width = length $ head grid
    height = length grid

findBlocksIgnoringBorder :: [[Bool]] -> [((Int, Int), (Int, Int))]
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

displayBlocks :: [[Bool]] -> [((Int, Int), (Int, Int))] -> IO ()
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
