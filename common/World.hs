module World where

import Data.List (sortBy)
import Data.Function (on)
import qualified Data.Map as M
import System.Random
import Control.Monad.Trans.State (runState)
-- import Protocol

import MCTS

type Point = (Int, Int)

(x, y) `vecp` (a, b) = (x+a, y+b)

norm :: Point -> Int
norm (x, y) =  max (abs x) (abs y)

dist :: Point -> Point -> Int
dist (x, y) (a, b) = max (abs $ x-a) (abs $ y-b)

data Object = Object {
              team :: Bool,
              coord :: Point,
              vel :: Point,
              temperature :: Int,
              stats :: Stats,
              maxTemp :: Int
              } deriving (Eq, Show)

type World = M.Map Int Object

data Stats = Stats
  { hitpoints :: Int
  , mana      :: Int
  , charisma  :: Int
  , telomeres :: Int
  }
  deriving (Eq, Ord, Show)

data Move
  = Boost Int Point
  | Detonate Int
  | Laser Int Point Int {- energy? -}
  | Mitosis Int Stats
  deriving (Eq, Ord, Show)

isBoost :: Move -> Bool
isBoost (Boost _ _) = True
isBoost _ = False

-- Must be sorted
type Moves = [Move]

moveShip :: Move -> World -> World
moveShip (Boost id pos) world =
    case M.lookup id world of
        Nothing -> world
        Just (Object t c v tp s mtp) -> M.insert id obj' world
            where v' = v `vecp` pos
                  tp' = tp -- TODO
                  s' = s -- TODO
                  obj' = Object t c v' tp' s' mtp
moveShip _ world = world

detonateShip :: Move -> World -> World
detonateShip (Detonate id) world = M.delete id world -- TODO: damage from explosion

damageStats :: Object -> Object
damageStats (Object t c v tp s mtp) = Object t c v tp' s' mtp
    where s' = if tp > mtp then Stats hp'' mn'' ch'' tl'' else s
          tp' = if tp > mtp then mtp else tp
          d = tp - mtp
          Stats hp mn ch tl = s
          hp' = hp-d
          mn' = if hp' < 0 then mn+hp' else mn
          ch' = if mn' < 0 then ch+hp' else ch
          tl' = if ch' < 0 then tl+ch' else tl
          hp'' = max 0 hp'
          mn'' = max 0 mn'
          ch'' = max 0 ch'
          tl'' = max 0 tl'

laserShip :: Move -> World -> World
laserShip (Laser id pos pow) world =
    case M.lookup id world of
        Nothing -> world
        Just (Object t c v tp s mtp) -> M.insert id obj' world
            where obj' = Object t c v (pow + tp) s' (tp + pow)
                  s' = Stats (hitpoints s) (mana s) (charisma s) (telomeres s)
laserShip _ world = world

mitosisShip :: Move -> World -> World
mitosisShip move world = world -- TODO

cooldownShip :: Object -> Object
cooldownShip (Object t c v tp s mtp) = Object t c v (max 0 (tp - charisma s)) s mtp

cooldownShips :: World -> World
cooldownShips = fmap cooldownShip

applyVel :: Object -> Object
applyVel (Object t c v tp s mtp) = Object t (c `vecp` v) v tp s mtp

applyGrav :: Object -> Object
applyGrav (Object t c v tp s mtp) = Object t c v' tp s mtp
    where (vx, vy) = v
          n = norm v
          gv = if n == 0 then (0, 0) else (vx `quot` n, vy `quot` n)
          v' = v `vecp` gv

updatePhysics :: World -> World
updatePhysics = fmap (applyVel . applyGrav)

doAction :: Move -> World -> World
doAction move@(Detonate _) = detonateShip move 
doAction move@(Laser _ _ _) = laserShip move
doAction move@(Mitosis _ _) = mitosisShip move

insideSq :: Int -> Point -> Bool
insideSq radius (x, y) = -radius <= x && x <= radius && -radius <= y && y <= radius

isAlive :: Int -> Int -> Object -> Bool
isAlive planet field (Object t c v tp s mtp) =
    not (insideSq planet c) &&
    insideSq field c && -- TODO
    telomeres s > 0

cleanWorld :: Int -> Int -> World -> World
cleanWorld planet field = M.filter (isAlive planet field)

-- Planet rad., field rad., ally moves, enemy moves, world.
tickWorld :: Int -> Int -> Moves -> Moves -> World -> World
tickWorld planet field moveA moveB world = world'
    where moves = moveA ++ moveB
          boosts = filter isBoost moves
          actions = filter (not . isBoost) moves
          worldMoved = (cleanWorld planet field) . updatePhysics $ foldr moveShip world boosts
          world' = foldr doAction worldMoved actions

enemyShips :: Bool -> World -> [(Int, Object)]
enemyShips myteam world = filter (\o -> team (snd o) /= myteam) $ M.toList world

possibleMoves :: Int -> Object -> World -> Moves
possibleMoves id obj world = det ++ dirs ++ lasers
    where myteam = team obj
          dirs = [Boost id (dx, dy) | dx <- [-1..1], dy <- [-1..1]]
          det = [Detonate id]
          enemyCoords = map (\(i, o) -> (i, (coord o) `vecp` (vel o))) $ enemyShips myteam world
          targets = [(i, (x+dx, y+dy)) | (i, (x, y)) <- enemyCoords, dx <- [-2..2], dy <- [-2..2]]
          lasers = [Laser i targ pow | (i, targ) <- targets, pow <- [1..mana (stats obj)]]

playerMoves :: Bool -> World -> Moves
playerMoves myteam world = concatMap (\(i, o) -> possibleMoves i o world) allyShips
    where allyShips = filter (\(i, o) -> team o == myteam) $ M.toList world

worldScore :: Bool -> World -> Double
worldScore myteam world =
    if myteam
    then (if ally > 0 then 1 else 0) -- Defender
    else (if enemy == 0 then 1 else 0) -- Attacker
    where ally = M.size $ M.filter (\o -> team o == myteam) world
          enemy = M.size $ M.filter (\o -> team o /= myteam) world

testMcts = MctsCtx (World.tickWorld 10 100) (worldScore True) (randomMove True) (randomMove False) uniformDouble
    where uniformDouble = getStdRandom . runState . uniformRMDouble
          randomMove myteam world = let moves = playerMoves myteam world
                                        len = length moves
                                    in do
                                        i <- randomRIO (0, len-1)
                                        if len == 0 then return [] else return [moves !! i]

testWorld :: World
testWorld = M.fromList [(0, obj1), (1, obj2)]
    where obj1 = Object True (10, 0) (0, 0) 0 (Stats 10 10 10 10) 10
          obj2 = Object False (-10, 0) (0, 0) 0 (Stats 10 10 10 10) 10

test a b = do
    m <- nTimes a (runMcts testMcts b) (initMcts testMcts testWorld)
    return $ selectBestMove m
