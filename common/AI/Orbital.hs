{-# LANGUAGE LambdaCase, NamedFieldPuns, BangPatterns #-}

module AI.Orbital where

import Data.Ratio
import Data.Maybe
import Data.List
import Data.Functor.Identity
import Data.Ord
import Data.Function (on)
import Control.Arrow

import Protocol

type Pos = (Int, Int)
type Vel = (Int, Int)
type Accel = (Int, Int)

simulateOrbit :: Applicative f => f Pos -> f Vel -> a -> (f Pos -> f Vel -> a -> Either b (a, f Accel)) -> b
simulateOrbit !pos !vel val boost
  = case boost pos vel val of
       Left ret -> ret
       Right (val', bs)
         -> let posvel' = estimateNextPos <$> pos <*> vel <*> bs in
            simulateOrbit (fst <$> posvel') (snd <$> posvel') val' boost

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

subPos :: Pos -> Pos -> Vel
subPos (x1, y1) (x2, y2) = (x1 - x2, y1 - y2)

l1Dist :: Pos -> Int
l1Dist (x, y) = max (abs x) (abs y)

opQuad :: L1Quadrant -> L1Quadrant
opQuad North = South
opQuad East = West
opQuad South = North
opQuad West = East

collidesWithConstantBoost :: Pos -> Vel -> RelativePos -> (Int -> Bool) -> Bool
collidesWithConstantBoost ipos ivel accel pred
  = simulateOrbit (Identity ipos) (Identity ivel) 0 $ \(Identity pos) (Identity vel) i ->
    if pred $ l1Dist pos
    then Left True
    else if i > 30
         then Left False
         else Right (i + 1, Identity $ relatePos accel $ l1Quadrant pos)

decideOrbital :: Pos -> Vel -> Int -> Accel
decideOrbital pos vel radius
  | not $ collidesWithConstantBoost pos vel (0, 0) (<= radius) =
    if not $ collidesWithConstantBoost pos vel (0, 0) (>= 128)
    then (0, 0)
    else if not $ collidesWithConstantBoost pos vel (-1, 0) (>= 128)
         then relatePos (-1, 0) quad
         else relatePos (-2, 0) quad
  | not $ collidesWithConstantBoost pos vel (0, tangential) (<= radius) = relatePos (0, tangential) quad
  | not $ collidesWithConstantBoost pos vel (1, tangential) (<= radius) = relatePos (1, tangential) quad
  | not $ collidesWithConstantBoost pos vel (1, 2 * tangential) (<= radius) = relatePos (1, 2 * tangential) quad
  | otherwise = relatePos (2, 2 * tangential) quad
  where
    quad = l1Quadrant pos
    tangential = if snd (unrelatePos vel quad) >= 0 then 1 else -1

data Pair a = Pair !a !a
instance Functor Pair where
  fmap f (Pair x y) = Pair (f x) (f y)
instance Applicative Pair where
  pure x = Pair x x
  Pair f g <*> Pair x y = Pair (f x) (g y)

decideSeeker :: Pos -> Vel -> Pos -> Vel -> Int -> (Accel, Int)
decideSeeker pos vel epos evel radius =
  minimumBy comp $ map (id &&& minAttainedDist) [(i, j) | i <- [-2..2], j <- [-2..2]]
  where
    minAttainedDist accel = simulateOrbit (Pair pos epos) (Pair vel evel) (0, l1Dist $ subPos pos epos)
      $ \(Pair pos epos) (Pair vel evel) (i, minDist) ->
        if l1Dist pos <= radius || l1Dist pos >= 128
        then Left 10000
        else if i > 10
          then Left $ min minDist $ l1Dist $ subPos pos epos
          else Right ((i + 1, min minDist $ l1Dist $ subPos pos epos), Pair (if i == 0 then accel else (0, 0)) (0, 0))
    comp (b1, m1) (b2, m2) = if m1 <= 4 && m2 <= 4
      then compare (l1Dist b1) (l1Dist b2)
      else compare m1 m2

produceInitialStats :: GameInfo -> IO Stats
produceInitialStats info = do
  let m = maxTotal $ maxStats info
  let defence = myTeam info == 1
  let ammo = if defence then 0 else 50
  let cooling = if defence then 0 else 7
  let telomeres = if defence then (m - 10) `div` 3 else 2
  pure $ Stats
    { fuel = (m - ammo * 4 - cooling * 12 - telomeres * 2)
    , ammo
    , cooling
    , telomeres
    }

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

expectedDamage :: Pos -> Int -> Int
expectedDamage (dx, dy) pow = floor $ (max 0 $ (3 * pow - dist) % 1) * dmgAngleRatio (dx, dy)
  where dist = max (abs dx) (abs dy)

dmgAngleRatio :: Pos -> Ratio Int
dmgAngleRatio (0, 0) = 1
dmgAngleRatio (dx, dy) = max 0 $ max (1 - (10 % 3) * (codist % dist)) (1 - (10 % 3) * (1 - codist % dist))
  where dist = max (abs dx) (abs dy)
        codist = min (abs dx) (abs dy)

produceMoves
  :: GameInfo
  -> [GameState] -- ^ head is most recent
  -> IO [Action]
produceMoves info (state:_) = pure $ telomereCommands ++ orbitalCommands ++ weaponCommands
  where
    ourTeam = myTeam info
    p2c (Coord x y) = (fromInteger x, fromInteger y)
    c2p (x, y) = Coord (toInteger x) (toInteger y)
    negatec2p (x, y) = Coord (toInteger (-x)) (toInteger (-y))
    radius = fromInteger $ planet info

    weaponCommands = mapMaybe weaponControl ourShips
    orbitalCommands = mapMaybe orbitalControl ourShips
    telomereCommands = if ourTeam == 1
                       then concatMap telomereControl ourShips ++ concatMap telomereControlSpread ourShips
                       else []

    randpm1 n = [-1, 0, 1] !! (fromIntegral (n `mod` 3))

    telomereControlSpread ship =
        let coinciding = filter (\s -> shipPos ship == shipPos s && shipVel ship == shipVel s) ourShips
            best = maximumBy (compare `on` (\s -> (fuel $ shipStats s) % (telomeres $ shipStats s))) coinciding
            ShipId id = shipId ship
        in if (orbitalControl ship == Nothing) -- && (ship == best)
           then [Boost (shipId ship) (Coord (randpm1 id) (randpm1 (id+1)))] -- not random 
           else []

    telomereControl ship | orbitalControl ship == Nothing = 
        [Mitosis (shipId ship) $ Stats (fuel (shipStats ship) `div` 2) (ammo (shipStats ship) `div` 2) (cooling (shipStats ship) `div` 2) 1]
                         | otherwise = []

    ourShips = filter ((ourTeam ==) . shipTeam) $ fst <$> gameShips state
    orbitalControl ship = case negatec2p $ decideOrbital (p2c $ shipPos ship) (p2c $ shipVel ship) radius of
      Coord 0 0 -> if isAttacker && shipTemp ship < shipMaxTemp ship `div` 2 {- seek -}
        then Just $ Boost (shipId ship) $ negatec2p $ fst $ decideSeeker (p2c $ shipPos ship) (p2c $ shipVel ship) (p2c $ shipPos enemy) (p2c $ shipVel enemy) radius
        else Nothing
      coord     -> Just $ Boost (shipId ship) coord

    enemy = head $ filter ((ourTeam /=) . shipTeam) $ fst <$> gameShips state
    (epos, _) = estimateNextPos (p2c $ shipPos enemy) (p2c $ shipVel enemy) (0, 0)

    weaponControl ship = case reverse $ sortOn snd [(pow, hitValue pow $ fromIntegral $ expectedDamage delta $ fromIntegral pow) | pow <- [0 .. min (ammo $ shipStats ship) (cooling (shipStats ship) + shipMaxTemp ship - shipTemp ship)]] of
      (pow, Just _):_ -> Just $ Laser (shipId ship) (c2p epos) pow
      _               -> Nothing
      where
        (mpos, _) = estimateNextPos (p2c $ shipPos ship) (p2c $ shipVel ship) $ decideOrbital (p2c $ shipPos ship) (p2c $ shipVel ship) radius
        delta = subPos mpos epos

    hitValue pow dmg
      | dmg <= 0 = Nothing -- pointless
      | dmg <= cooled = Nothing -- absorbed by coolant
      | dmg - cooled <= tempBuffer = if pow > dmg - cooled -- heats, no damage
          then Nothing -- heats us more than them
          else Just (1, dmg) -- heats them more than us
      | dmg - cooled - tempBuffer < hitpoints = Just (2, dmg) -- damage, no kill
      | dmg - cooled - tempBuffer - hitpoints < 10 = Just (3, dmg) -- kill (add 10 extra to be sure)
      | otherwise = Nothing -- overkill
      where cooled = cooling (shipStats enemy)
            tempBuffer = shipMaxTemp enemy - shipTemp enemy
            hitpoints = fuel (shipStats enemy) + ammo (shipStats enemy) + cooling (shipStats enemy) + telomeres (shipStats enemy)

    isAttacker = ourTeam == 0
