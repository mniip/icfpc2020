module Simulation where

import Protocol

type Point = (Integer, Integer)

data Object = Object {
              coord :: Point,
              vel :: Point
              } deriving (Eq, Show)

data World = World {
             team :: Integer,
             planetR :: Integer,
             fieldR :: Integer,
             objects :: [Object]
             } deriving (Show)

normalize :: Point -> Point
normalize v@(x, y) = if m == 0 then v else (x `quot` m, y `quot` m)
    where m = max (abs x) (abs y)

addGrav :: Object -> Object
addGrav (Object c@(x, y) v@(vx, vy)) = Object c v'
    where m = max (abs x) (abs y)
          dx = x `quot` m
          dy = y `quot` m
          v' = if m == 0 then v else (vx-dx, vy-dy)

moveObject :: Object -> Object
moveObject (Object (x, y) (vx, vy)) = Object (x+vx, y+vy) (vx, vy)

orbit :: Object -> Object
orbit = moveObject . addGrav

inRect :: Point -> (Point, Point) -> Bool
inRect (x, y) ((lx, ly), (rx, ry)) = lx <= x && x <= rx && ly <= y && y <= ry

inBounds :: World -> Object -> Bool
inBounds (World _ pl fld _) (Object c _) = inRect c plrect && inRect c fldrect
    where plrect = ((-pl, -pl), (pl, pl))
          fldrect = ((-fld, -fld), (fld, fld))

goodOrbit :: World -> Object -> Bool
goodOrbit world c0 = not . null $ filter (== c0) aliveStates
    where aliveStates = takeWhile (inBounds world) . tail . take 100 $ iterate orbit c0

findOrbitVel :: World -> Point -> Maybe Point
findOrbitVel world p = if null variants then Nothing else return (head variants)
    where variants = filter (\v -> goodOrbit world (Object p v)) [(x, y) | x <- [-10..10], y <- [-10..10]]

correctOrbit :: World -> Object -> Point
correctOrbit world (Object pos@(x, y) (vx, vy)) =
    if not (null vars) then head vars else awayAndClockwise
    where vs = [(dx, dy) | dx <- [-1..1], dy <- [-1..1]]
          vars = filter (\(dx, dy) -> goodOrbit world (Object pos (vx+dx, vy+dy))) vs
          rotate (a, b) = (b, -a)
          awayAndClockwise = rotate $ normalize pos

produceInitialStats :: GameInfo -> IO Stats
produceInitialStats gi = return $ Stats (total-2) 0 0 1
    where total = maxTotal $ maxStats gi

coordToPoint :: Coord -> Point
coordToPoint (Coord x y) = (x, y)

produceMoves :: GameInfo -> [GameState] -> IO [Action]
produceMoves gi (gs:_) = return $ map shipAction myShips
    where t = myTeam gi
          wrld = World t (planet gi) (field gi) []
          ally sh = shipTeam sh == t
          myShips = filter ally . map fst $ gameShips gs
          shipAction sh = let obj = Object (coordToPoint $ shipPos sh) (coordToPoint $ shipVel sh)
                              (bx, by) = correctOrbit wrld obj
                          in Boost (shipId sh) (Coord bx by)
