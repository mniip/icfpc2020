module World where

import Data.List (sortBy)
import Data.Function (on)
import qualified Data.Map as M
-- import Protocol

type Point = (Int, Int)

(x, y) `vecp` (a, b) = (x+a, y+b)

dist :: Point -> Point -> Int
dist (x, y) (a, b) = max (abs $ x-a) (abs $ y-b)

data Object = Object {
              team :: Bool,
              coord :: Point,
              vel :: Point,
              stats :: Stats,
              maxTemp :: Int
              } deriving (Eq, Show)

type World = M.Map Int Object

data Stats = Stats
  { hitpoints :: Int
  , mana      :: Int
  , charisma  :: Int
  , telomeres :: Int
  , temperature :: Int
  }
  deriving (Eq, Ord, Show)

data Move
  = Boost Int Point
  | Detonate Int
  | Laser Int Point Int {- energy? -}
  | Mitosis Int Stats
  deriving (Eq, Ord, Show)

-- Must be sorted
type Moves = [Move]

moveShip :: Move -> World -> World
moveShip (Boost id pos) world =
    case M.lookup id world of
        Nothing -> world
        Just (Object t c v s mtp) -> M.insert id obj' world
            where v' = v `vecp` pos
                  obj' = Object t c v' s mtp
moveShip _ world = world

detonateShip :: Int -> World -> World
detonateShip id world = M.delete id world -- TODO: damage from explosion

damageStats :: Object -> Object
damageStats (Object t c v s mtp) = Object t c v s' mtp
    where s' = if tp > mtp then Stats hp'' mn'' ch'' tl'' mtp else s
          d = tp - mtp
          Stats hp mn ch tl tp = s
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
        Just (Object t c v s tp) -> M.insert id obj' world
            where obj' = Object t c v s' (tp+pow)
                  s' = Stats (hitpoints s) (mana s) (charisma s) (telomeres s) (pow + temperature s)
laserShip _ world = world

tickWorld :: Moves -> Moves -> World -> World
tickWorld moveA moveB world = undefined
