{-# LANGUAGE BangPatterns, RecordWildCards, LambdaCase, NamedFieldPuns #-}

module MCTS where

import Control.Comonad.Store
import qualified Data.Map.Strict as M
import System.Random
import Data.List
import Data.Ord
import Control.Monad.Trans.State
import Data.Word

-- score = sum ourHp / (sum ourHp + sum theirHp)
-- 1 = win, 0 = loss
-- coScore = sum theirHp / (sum ourHp + sum theirHp)
-- = 1 - score

type Score = Double

data Mcts world moveA moveB = Mcts
  { worldState      :: !world
  , currentScore    :: !Double
  , subTree         :: !(M.Map moveA (CoMcts world moveA moveB))
    -- empty if win or loss
  , totalScore      :: !Double -- ^ Includes self
    -- = currentScore + sum (map coTotalScore subTree)
  , numLeaves       :: !Int -- ^ Includes self
    -- = 1 + sum (map coNumLeaves subTree)
  }
  deriving (Eq, Show)

data CoMcts world moveA moveB = CoMcts
  { coSubTree      :: !(M.Map moveB (Mcts world moveA moveB))
    -- not empty
  , coTotalScore   :: !Double
    -- = sum (map totalScore coSubTree)
  , coNumLeaves    :: !Int
    -- = sum (map numLeaves coSubTree)
  }
  deriving (Eq, Show)

mctsAverageScore :: Mcts world moveA moveB -> Double
mctsAverageScore Mcts{..} = totalScore / fromIntegral numLeaves

mctsAverageCoScore :: Mcts world moveA moveB -> Double
mctsAverageCoScore Mcts{..} = (fromIntegral numLeaves - totalScore) / fromIntegral numLeaves

coMctsAverageScore :: CoMcts world moveA moveB -> Double
coMctsAverageScore CoMcts{..} = coTotalScore / fromIntegral coNumLeaves

coMctsAverageCoScore :: CoMcts world moveA moveB -> Double
coMctsAverageCoScore CoMcts{..} = (fromIntegral coNumLeaves - coTotalScore) / fromIntegral coNumLeaves

-- Nothing = branch from this tree
selectAMove :: RandomGen g => Mcts world moveA moveB -> State g (Maybe moveA)
selectAMove Mcts{..} = makeChoice (M.toList subTree) <$> uniformRMDouble (0, totalScore)
  where
    makeChoice [] !p      = Nothing
    makeChoice ((move, CoMcts{..}):cms) !p
      | p <= coTotalScore = Just move
      | otherwise         = makeChoice cms (p - coTotalScore)

selectBMove :: RandomGen g => CoMcts world moveA moveB -> State g moveB
selectBMove CoMcts{..} = makeChoice (M.toList coSubTree) <$> uniformRMDouble (0, fromIntegral coNumLeaves - coTotalScore)
  where
    makeChoice [(move, _)] !p = move
    makeChoice ((move, Mcts{..}):ms) !p
      | p <= fromIntegral numLeaves - totalScore       = move
      | otherwise             = makeChoice ms (p - (fromIntegral numLeaves - totalScore))

selectMoves :: (Ord moveA, Ord moveB, RandomGen g) => Mcts world moveA moveB -> State g (Store (Mcts world moveA moveB) (Mcts world moveA moveB))
selectMoves m@Mcts{..} = selectAMove m >>= \case
  Just moveA -> do
    let cm = subTree M.! moveA
    coW <- selectCoMoves cm
    let updateM cm' = Mcts
          { worldState
          , currentScore
          , subTree = M.insert moveA cm' subTree
          , totalScore = totalScore + coTotalScore cm' - coTotalScore cm
          , numLeaves = numLeaves + coNumLeaves cm' - coNumLeaves cm
          }
    pure $ fmap updateM coW
  Nothing -> pure $ store id m

selectCoMoves :: (Ord moveA, Ord moveB, RandomGen g) => CoMcts world moveA moveB -> State g (Store (Mcts world moveA moveB) (CoMcts world moveA moveB))
selectCoMoves cm@CoMcts{..} = do
  moveB <- selectBMove cm
  let m = coSubTree M.! moveB
  w <- selectMoves m
  let updateCM m' = CoMcts
        { coSubTree = M.insert moveB m' coSubTree
        , coTotalScore = coTotalScore + totalScore m' - totalScore m
        , coNumLeaves = coNumLeaves + numLeaves m' - numLeaves m
        }
  pure $ fmap updateCM w

createMoves :: (Ord moveA, Ord moveB, Monad m) => (world -> m moveA) -> (world -> m moveB) -> (moveA -> moveB -> world -> world) -> (world -> Double) -> world -> Double -> m (Mcts world moveA moveB)
createMoves mkA mkB tick score w s
  | s == 0.0 || s == 1.0 = pure $ Mcts
                                  { worldState = w
                                  , currentScore = s
                                  , subTree = M.empty
                                  , totalScore = s
                                  , numLeaves = 1
                                  }
  | otherwise = do
      moveA <- mkA w
      cm <- createCoMoves mkA mkB tick score w moveA
      pure $ Mcts
             { worldState = w
             , currentScore = s
             , subTree = M.singleton moveA cm
             , totalScore = s + coTotalScore cm
             , numLeaves = 1 + coNumLeaves cm
             }

createCoMoves :: (Ord moveA, Ord moveB, Monad m) => (world -> m moveA) -> (world -> m moveB) -> (moveA -> moveB -> world -> world) -> (world -> Double) -> world -> moveA -> m (CoMcts world moveA moveB)
createCoMoves mkA mkB tick score w moveA = do
  moveB <- mkB w
  let w' = tick moveA moveB w
  m <- createMoves mkA mkB tick score w' (score w')
  pure $ CoMcts
         { coSubTree = M.singleton moveB m
         , coTotalScore = totalScore m
         , coNumLeaves = numLeaves m
         }

tryBranchMove :: (Ord moveA, Ord moveB, Monad m) => (world -> m moveA) -> (world -> m moveB) -> (moveA -> moveB -> world -> world) -> (world -> Double) -> Mcts world moveA moveB -> m (Mcts world moveA moveB)
tryBranchMove mkA mkB tick score Mcts{..} = do
  moveA <- mkA worldState
  case M.lookup moveA subTree of
    Nothing -> do
      cm <- createCoMoves mkA mkB tick score worldState moveA
      pure $ Mcts
        { worldState
        , currentScore
        , subTree = M.insert moveA cm subTree
        , totalScore = totalScore + coTotalScore cm
        , numLeaves = numLeaves + coNumLeaves cm
        }
    Just cm -> do
      cm' <- tryBranchCoMove mkA mkB tick score worldState moveA cm
      pure $ Mcts
        { worldState
        , currentScore
        , subTree = M.insert moveA cm' subTree
        , totalScore = totalScore + coTotalScore cm' - coTotalScore cm
        , numLeaves = numLeaves + coNumLeaves cm' - coNumLeaves cm
        }

tryBranchCoMove :: (Ord moveA, Ord moveB, Monad m) => (world -> m moveA) -> (world -> m moveB) -> (moveA -> moveB -> world -> world) -> (world -> Double) -> world -> moveA -> CoMcts world moveA moveB -> m (CoMcts world moveA moveB)
tryBranchCoMove mkA mkB tick score w moveA cm@CoMcts{..} = do
  moveB <- mkB w
  case M.lookup moveB coSubTree of
    Nothing -> do
      let w' = tick moveA moveB w
      m <- createMoves mkA mkB tick score w' (score w')
      pure $ CoMcts
        { coSubTree = M.insert moveB m coSubTree
        , coTotalScore = coTotalScore + totalScore m
        , coNumLeaves = coNumLeaves + numLeaves m
        }
    _ -> pure cm {- no new moves -}

initMcts :: (Monad m, Ord moveA, Ord moveB) => (world -> m moveA) -> (world -> m moveB) -> (moveA -> moveB -> world -> world) -> (world -> Double) -> world -> m (Mcts world moveA moveB)
initMcts mkA mkB tick score w = createMoves mkA mkB tick score w (score w)

runMcts :: (Monad m, RandomGen g, Ord moveA, Ord moveB) => (world -> m moveA) -> (world -> m moveB) -> (moveA -> moveB -> world -> world) -> (world -> Double) -> Mcts world moveA moveB -> g -> m (Mcts world moveA moveB, g)
runMcts mkA mkB tick score m g = case runState (selectMoves m) g of
  (w, g') -> do
    m' <- experiment (tryBranchMove mkA mkB tick score) w
    pure (m', g)

selectBestMove :: Mcts world moveA moveB -> moveA
selectBestMove Mcts{..} = fst $ maximumBy (comparing $ coMctsAverageScore . snd) $ M.toList subTree

-- From random-1.2.0

uniformRMDouble :: RandomGen g => (Double, Double) -> State g Double
uniformRMDouble (l, h)
  | l == h = return l
  | otherwise = do
      x <- uniformDouble01M
      return $ x * l + (1 - x) * h

uniformDouble01M :: RandomGen g => State g Double
uniformDouble01M = do
  w64 <- state random
  return $ fromIntegral (w64 :: Word64) / m
    where
      m = fromIntegral (maxBound :: Word64) :: Double


