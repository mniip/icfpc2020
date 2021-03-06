{-# LANGUAGE BangPatterns, RecordWildCards, LambdaCase, NamedFieldPuns #-}

module MCTS where

import Control.Arrow
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

data MctsCtx m world moveA moveB = MctsCtx
  { tickWorld :: moveA -> moveB -> world -> world
  , calcScore :: world -> Double
  , randomMoveA :: world -> m moveA
  , randomMoveB :: world -> m moveB
  , uniformDouble :: (Double, Double) -> m Double
  }

selectBestAMove :: Mcts world moveA moveB -> Maybe moveA
selectBestAMove Mcts{..} = fst $ maximumBy (comparing snd) $ (Nothing, currentScore) : map (Just *** coMctsAverageScore) (M.toList subTree)

selectWorstBMove :: CoMcts world moveA moveB -> moveB
selectWorstBMove CoMcts{..} = fst $ maximumBy (comparing $ mctsAverageCoScore . snd) $ M.toList coSubTree

-- Nothing = branch from this tree
selectWeightedAMove :: Monad m => MctsCtx m world moveA moveB -> Mcts world moveA moveB -> m (Maybe moveA)
selectWeightedAMove MctsCtx{..} Mcts{..} = makeChoice (M.toList subTree) <$> uniformDouble (0, totalScore)
  where
    makeChoice [] !p      = Nothing
    makeChoice ((move, CoMcts{..}):cms) !p
      | p <= coTotalScore = Just move
      | otherwise         = makeChoice cms (p - coTotalScore)

selectWeightedBMove :: Monad m => MctsCtx m world moveA moveB -> CoMcts world moveA moveB -> m moveB
selectWeightedBMove MctsCtx{..} CoMcts{..} = makeChoice (M.toList coSubTree) <$> uniformDouble (0, fromIntegral coNumLeaves - coTotalScore)
  where
    makeChoice [(move, _)] !p = move
    makeChoice ((move, Mcts{..}):ms) !p
      | p <= fromIntegral numLeaves - totalScore       = move
      | otherwise             = makeChoice ms (p - (fromIntegral numLeaves - totalScore))

selectMoves :: (Ord moveA, Ord moveB, Monad m) => MctsCtx m world moveA moveB -> Mcts world moveA moveB -> m (Store (Mcts world moveA moveB) (Mcts world moveA moveB))
selectMoves ctx m@Mcts{..} = pure (selectBestAMove m) >>= \case
  Just moveA -> do
    let cm = subTree M.! moveA
    coW <- selectCoMoves ctx cm
    let updateM cm' = Mcts
          { worldState
          , currentScore
          , subTree = M.insert moveA cm' subTree
          , totalScore = totalScore + coTotalScore cm' - coTotalScore cm
          , numLeaves = numLeaves + coNumLeaves cm' - coNumLeaves cm
          }
    pure $ fmap updateM coW
  Nothing -> pure $ store id m

selectCoMoves :: (Ord moveA, Ord moveB, Monad m) => MctsCtx m world moveA moveB -> CoMcts world moveA moveB -> m (Store (Mcts world moveA moveB) (CoMcts world moveA moveB))
selectCoMoves ctx cm@CoMcts{..} = do
  moveB <- pure $ selectWorstBMove cm
  let m = coSubTree M.! moveB
  w <- selectMoves ctx m
  let updateCM m' = CoMcts
        { coSubTree = M.insert moveB m' coSubTree
        , coTotalScore = coTotalScore + totalScore m' - totalScore m
        , coNumLeaves = coNumLeaves + numLeaves m' - numLeaves m
        }
  pure $ fmap updateCM w

createMoves :: (Ord moveA, Ord moveB, Monad m) => MctsCtx m world moveA moveB -> world -> Double -> m (Mcts world moveA moveB)
createMoves ctx@MctsCtx{..} w s
  | s == 0.0 || s == 1.0 = pure $ Mcts
                                  { worldState = w
                                  , currentScore = s
                                  , subTree = M.empty
                                  , totalScore = s
                                  , numLeaves = 1
                                  }
  | otherwise = do
      moveA <- randomMoveA w
      cm <- createCoMoves ctx w moveA
      pure $ Mcts
             { worldState = w
             , currentScore = s
             , subTree = M.singleton moveA cm
             , totalScore = s + coTotalScore cm
             , numLeaves = 1 + coNumLeaves cm
             }

createCoMoves :: (Ord moveA, Ord moveB, Monad m) => MctsCtx m world moveA moveB -> world -> moveA -> m (CoMcts world moveA moveB)
createCoMoves ctx@MctsCtx{..} w moveA = do
  moveB <- randomMoveB w
  let w' = tickWorld moveA moveB w
  m <- createMoves ctx w' (calcScore w')
  pure $ CoMcts
         { coSubTree = M.singleton moveB m
         , coTotalScore = totalScore m
         , coNumLeaves = numLeaves m
         }

tryBranchMove :: (Ord moveA, Ord moveB, Monad m) => MctsCtx m world moveA moveB -> Mcts world moveA moveB -> m (Mcts world moveA moveB)
tryBranchMove ctx@MctsCtx{..} Mcts{..} = do
  moveA <- randomMoveA worldState
  case M.lookup moveA subTree of
    Nothing -> do
      cm <- createCoMoves ctx worldState moveA
      pure $ Mcts
        { worldState
        , currentScore
        , subTree = M.insert moveA cm subTree
        , totalScore = totalScore + coTotalScore cm
        , numLeaves = numLeaves + coNumLeaves cm
        }
    Just cm -> do
      cm' <- tryBranchCoMove ctx worldState moveA cm
      pure $ Mcts
        { worldState
        , currentScore
        , subTree = M.insert moveA cm' subTree
        , totalScore = totalScore + coTotalScore cm' - coTotalScore cm
        , numLeaves = numLeaves + coNumLeaves cm' - coNumLeaves cm
        }

tryBranchCoMove :: (Ord moveA, Ord moveB, Monad m) => MctsCtx m world moveA moveB -> world -> moveA -> CoMcts world moveA moveB -> m (CoMcts world moveA moveB)
tryBranchCoMove ctx@MctsCtx{..} w moveA cm@CoMcts{..} = do
  moveB <- randomMoveB w
  case M.lookup moveB coSubTree of
    Nothing -> do
      let w' = tickWorld moveA moveB w
      m <- createMoves ctx w' (calcScore w')
      pure $ CoMcts
        { coSubTree = M.insert moveB m coSubTree
        , coTotalScore = coTotalScore + totalScore m
        , coNumLeaves = coNumLeaves + numLeaves m
        }
    _ -> pure cm {- no new moves -}

initMcts :: (Monad m, Ord moveA, Ord moveB) => MctsCtx m world moveA moveB -> world -> Mcts world moveA moveB
initMcts ctx@MctsCtx{..} w = Mcts
  { worldState = w
  , currentScore = calcScore w
  , subTree = M.empty
  , totalScore = calcScore w
  , numLeaves = 1
  }

runMcts :: (Monad m, Ord moveA, Ord moveB) => MctsCtx m world moveA moveB -> Int -> Mcts world moveA moveB -> m (Mcts world moveA moveB)
runMcts ctx rollouts m = experiment (nTimes rollouts $ tryBranchMove ctx) =<< selectMoves ctx m

nTimes :: Monad m => Int -> (a -> m a) -> a -> m a
nTimes 0 _ x = pure x
nTimes n f x = f x >>= nTimes (n - 1) f

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
