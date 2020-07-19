type Point = (Int, Int)

data Object = Object {
              coord :: Point,
              vel :: Point
              } deriving (Eq, Show)

data World = World {
             planet :: Int,
             objects :: [Object]
             } deriving (Show)

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

goodOrbit :: Object -> Bool
goodOrbit c0 = not . null . filter (== c0) . tail . take 100 $ iterate orbit c0

findOrbitVel :: Point -> Maybe Point
findOrbitVel p = if null variants then Nothing else return (head variants)
    where variants = filter (\v -> goodOrbit (Object p v)) [(x, y) | x <- [-10..10], y <- [-10..10]]

correctOrbit :: Object -> Point
correctOrbit (Object pos@(x, y) (vx, vy)) = if not (null vars) then head vars else undefined
    where vs = [(vx+dx, vy+dy) | dx <- [-1..1], dy <- [-1..1]]
          vars = filter (\v -> goodOrbit (Object pos v)) vs
