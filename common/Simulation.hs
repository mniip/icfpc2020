module AI.Simulation where

import Data.List (sortBy)
import Data.Function (on)
import Protocol

type Point = (Integer, Integer)

data Object = Object {
              team :: Bool,
              coord :: Point,
              vel :: Point
              } deriving (Eq, Show)

data World = World {
             teamB :: Bool,
             planetR :: Integer,
             fieldR :: Integer,
             objects :: [Object]
             } deriving (Show)

magn :: Point -> Integer
magn (x, y) = max (abs x) (abs y)

normalize :: Point -> Point
normalize v@(x, y) = if m == 0 then v else (x `quot` m, y `quot` m)
    where m = magn v

addGrav :: Object -> Object
addGrav (Object b c@(x, y) v@(vx, vy)) = Object b c v'
    where m = magn c
          dx = x `quot` m
          dy = y `quot` m
          v' = if m == 0 then v else (vx-dx, vy-dy)

moveObject :: Object -> Object
moveObject (Object b (x, y) (vx, vy)) = Object b (x+vx, y+vy) (vx, vy)

orbit :: Object -> Object
orbit = moveObject . addGrav

inRect :: Point -> (Point, Point) -> Bool
inRect (x, y) ((lx, ly), (rx, ry)) = lx <= x && x <= rx && ly <= y && y <= ry

inBounds :: World -> Object -> Bool
inBounds (World _ pl fld _) (Object _ c _) = inRect c plrect && inRect c fldrect
    where plrect = ((-pl, -pl), (pl, pl))
          fldrect = ((-fld, -fld), (fld, fld))

goodOrbit :: World -> Object -> Bool
goodOrbit world c0 = not . null $ filter (== c0) aliveStates
    where aliveStates = takeWhile (inBounds world) . tail . take 100 $ iterate orbit c0

findOrbitVel :: World -> Point -> Maybe Point
findOrbitVel world p = if null variants then Nothing else return (head variants)
    where variants = filter (\v -> goodOrbit world (Object True p v)) [(x, y) | x <- [-10..10], y <- [-10..10]]

neg (a, b) = (-a, -b)

correctOrbit :: World -> Object -> Point
correctOrbit world (Object _ pos@(x, y) vel@(vx, vy)) =
    if not (null vars) then head vars else defaultMove
    where vs = [(dx, dy) | dx <- [-1..1], dy <- [-1..1]]
          vars = filter (\(dx, dy) -> goodOrbit world (Object True pos (vx+dx, vy+dy))) vs
          rotate (a, b) = (b, -a)
          grav = normalize pos
          orbitvel = case findOrbitVel world pos of
                         Nothing -> (0, 0)
                         Just v -> v
          veldiff = orbitvel `vecp` vel
          minxy = min (abs vx) (abs vy)
          maxxy = max (abs vx) (abs vy)
          alpha = (fromIntegral maxxy) / (fromIntegral $ minxy^2)
          defaultMove | alpha < 0.02 || (abs x == abs y) || magn pos > 3*(planetR world) = (0, 0)
                      | abs x < abs y = (1, -(y `quot` (abs y)))
                      | abs x > abs y = (-(x `quot` (abs x)), 1)
                      | otherwise = neg $ normalize pos -- neg ((normalize $ (y, -x)) `vecp` grav)

vecp (a, b) (c, d) = (a+c, b+d)

initWorld :: GameInfo -> [Object] -> World
initWorld gi objs = World (myTeam gi == 1) (planet gi) (field gi) objs

shipToObject :: Ship -> Object
shipToObject sh = Object (shipTeam sh == 1) (coordToPoint $ shipPos sh) (coordToPoint $ shipVel sh)

cleanWorld :: World -> World
cleanWorld ws@(World b p f objs) = World b p f (filter (inBounds ws) objs)

updateWorld :: World -> World
updateWorld (World t p f objs) = cleanWorld (World t p f (map (moveObject . addGrav) objs))

commandAlly :: World -> (Object -> Object) -> World
commandAlly (World b p f objs) update = World b p f (map go objs)
    where go o = if team o == b then update o else o

applyAcc :: Object -> Point -> Object
applyAcc (Object b p v) dv = Object b p (v `vecp` dv)

produceInitialStats :: GameInfo -> IO Stats
produceInitialStats gi = return $ Stats (total - mn * 4 - ch * 12 - tm * 2) mn ch tm
    where total = maxTotal $ maxStats gi
          mn = 50
          ch = 7
          tm = 1

coordToPoint :: Coord -> Point
coordToPoint (Coord x y) = (x, y)

pointToCoord :: Point -> Coord
pointToCoord (x, y) = Coord x y

produceMoves :: GameInfo -> [GameState] -> IO [Action]
produceMoves gi (gs:_) = return $ concatMap shipAction myShips
    where t = myTeam gi
          wrld = World (t == 1) (planet gi) (field gi) []
          ally sh = shipTeam sh == t
          myShips = filter ally . map fst $ gameShips gs
          enemyShips = filter (not . ally) . map fst $ gameShips gs
          shipAction sh = 
              let obj = Object (t == 1) pos vel
                  pos = coordToPoint $ shipPos sh
                  grav = neg $ normalize pos
                  vel = coordToPoint $ shipVel sh
                  pos' = pos `vecp` vel `vecp` grav
                  (bx, by) = correctOrbit wrld obj
                  idd = shipId sh
                  enemy = head enemyShips
                  epos = coordToPoint $ shipPos enemy
                  evel = coordToPoint $ shipVel enemy
                  epos' = epos `vecp` evel `vecp` grav
                  edist = magn (epos `vecp` (neg pos'))
                  fire | (not $ null enemyShips) && (t == 0) = if edist <= 4 then [Detonate idd] else [Laser idd (pointToCoord epos') 7]
                       | otherwise = []
              in [Boost idd (Coord bx by)] ++ fire
