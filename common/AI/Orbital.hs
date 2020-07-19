{-# LANGUAGE LambdaCase #-}

module AI.Orbital where

import Data.Ratio
import Data.Maybe

import Protocol

type Pos = (Int, Int)
type Vel = (Int, Int)
type Accel = (Int, Int)

simulateOrbit :: Pos -> Vel -> a -> (Pos -> Vel -> a -> Either b (a, Accel)) -> b
simulateOrbit pos vel val boost
  = case boost pos vel val of
       Left ret -> ret
       Right (val', bs)
         -> case estimateNextPos pos vel bs of
              (pos', vel') -> simulateOrbit pos' vel' val' boost

data L1Quadrant = North | East | South | West deriving (Eq, Show)

-- NW, NE -> N; SE, SW -> S
l1Quadrant :: (Int, Int) -> L1Quadrant
l1Quadrant (x, y) = if abs x > abs y
  then if x > 0 then East else West
  else if y > 0 then South else North

lInftyRadius :: (Int, Int) -> Int
lInftyRadius (x, y) = max (abs x) (abs y)

type RelativePos = (Int {- how much outwards -}, Int {- how much CW -})

relatePos :: RelativePos -> L1Quadrant -> Pos
relatePos (out, cw) = \case
  North -> (cw, -out)
  East -> (out, cw)
  South -> (-cw, out)
  West -> (-out, -cw)

-- unrelatePos p (relatePos p q) q = p
-- relatePos p (unrelatePos p q) q = p
unrelatePos :: Pos -> L1Quadrant -> RelativePos
unrelatePos (x, y) = \case
  North -> (-y, x)
  East -> (x, y)
  South -> (y, -x)
  West -> (-x, -y)

manhattanDist :: Pos -> Int
manhattanDist (x, y) = max (abs x) (abs y)

collidesWithConstantBoost :: Pos -> Vel -> Accel -> Int -> Bool
collidesWithConstantBoost ipos ivel accel radius
  = simulateOrbit ipos ivel () $ \pos vel () ->
    if manhattanDist pos <= radius
    then Left True
    else if l1Quadrant pos /= initQuad
         then Left False
         else Right ((), accel)
  where
    initQuad = l1Quadrant ipos

decideOrbital :: Pos -> Vel -> Int -> Accel
decideOrbital pos vel radius
  | not $ collidesWithConstantBoost pos vel (0, 0) radius = (0, 0)
  | not $ collidesWithConstantBoost pos vel (relatePos (0, tangential) quad) radius = relatePos (0, tangential) quad
  | not $ collidesWithConstantBoost pos vel (relatePos (1, tangential) quad) radius = relatePos (1, tangential) quad
  | not $ collidesWithConstantBoost pos vel (relatePos (1, 2 * tangential) quad) radius = relatePos (1, 2 * tangential) quad
  | otherwise = relatePos (2, 2 * tangential) quad
  where
    quad = l1Quadrant pos
    tangential = if snd (unrelatePos vel quad) >= 0 then 1 else -1


produceInitialStats :: GameInfo -> IO Stats
produceInitialStats info = do
  let m = maxTotal $ maxStats info
  let telomeres = 1
  let mana = 50
  let charisma = 7
  pure $ Stats (m - mana * 4 - charisma * 12 - telomeres * 2) mana charisma telomeres

estimateNextPos :: Pos -> Vel -> Accel -> (Pos, Vel)
estimateNextPos (x, y) (vx, vy) (bx, by)
  = let
      gravx = -signum x * (if abs x >= abs y then 1 else 0)
      gravy = -signum y * (if abs y >= abs x then 1 else 0)

      vx' = vx + gravx + bx
      vy' = vy + gravy + by

      x' = x + vx'
      y' = y + vy'
    in ((x', y'), (vx', vy'))

dmgRatio :: Pos -> Ratio Int
dmgRatio (dx, dy) = max 0 $ max (1 - (10 % 3) * (codist % dist)) (1 - (10 % 3) * (1 - codist % dist))
  where dist = max (abs dx) (abs dy)
        codist = min (abs dx) (abs dy)

produceMoves
  :: GameInfo
  -> [GameState] -- ^ head is most recent
  -> IO [Action]
produceMoves info (state:_) = pure $ map orbitalControl ourShips ++ mapMaybe weaponControl ourShips
  where
    ourTeam = myTeam info
    p2c (Coord x y) = (fromInteger x, fromInteger y)
    c2p (x, y) = Coord (toInteger x) (toInteger y)
    negatec2p (x, y) = Coord (toInteger (-x)) (toInteger (-y))
    radius = fromInteger $ planet info

    ourShips = filter ((ourTeam ==) . shipTeam) $ fst <$> gameShips state
    orbitalControl ship = Boost (shipId ship) $ negatec2p $ decideOrbital (p2c $ shipPos ship) (p2c $ shipVel ship) radius

    enemy = head $ filter ((ourTeam /=) . shipTeam) $ fst <$> gameShips state
    (epos, _) = estimateNextPos (p2c $ shipPos enemy) (p2c $ shipVel enemy) (0, 0)

    weaponControl ship = let diffRatio = dmgRatio (epos `subpos` mpos) - (fromInteger $ shipTemp ship) % (fromInteger $ 3 + shipMaxTemp ship) in
      if diffRatio > 0
      then Just $ Laser (shipId ship) (c2p epos) (floor $ (fromIntegral $ 3 + shipMaxTemp ship) * diffRatio)
      else Nothing
      where (mpos, _) = estimateNextPos (p2c $ shipPos ship) (p2c $ shipVel ship) $ decideOrbital (p2c $ shipPos ship) (p2c $ shipVel ship) radius

    subpos (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)
