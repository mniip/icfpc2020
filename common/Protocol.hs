{-# LANGUAGE RecordWildCards, LambdaCase #-}

module Protocol where

import Control.Monad
import Data.Char
import Data.List
import Control.Monad
import Control.Applicative
import Data.Maybe

import Common

data MStruct = MList [MStruct] | MInt Integer | MCons (MStruct, MStruct) deriving (Read, Show)

change :: String -> String
change [] = []
change ('(':xs) = "MCons (" ++ change xs
change ('[':xs) = "MList [" ++ change xs
change list@(x:xs) | x == ')' || x == ']' = x : change xs
                   | isNum x = let (num, rest) = span isNum list
                               in "MInt " ++ num ++ change rest
                   | otherwise = x : change xs
    where isNum y = y `elem` "+-0123456789"

parseMStruct :: String -> MStruct
parseMStruct str = read $ change str'
    where str' = filter (not . isSpace) str

parseShortList :: String -> IntList
parseShortList = toIntList . parseMStruct
    where listToIntList [] = LNil
          listToIntList (x:xs) = LCons (toIntList x) (listToIntList xs)
          toIntList (MInt n) = LInt n
          toIntList (MCons (a, b)) = LCons (toIntList a) (toIntList b)
          toIntList (MList xs) = listToIntList xs

pprList = go []
  where go els LNil = "[" ++ intercalate "," (pprList <$> reverse els) ++ "]"
        go els (LCons x xs) = go (x:els) xs
        go [] (LInt i) = show i
        go els x = "(" ++ intercalate "," (pprList <$> reverse (x:els)) ++ ")"

class Protocol a where
  toProto :: a -> IntList
  fromProto :: IntList -> Maybe a

anInt :: IntList -> Maybe Integer
anInt (LInt x) = pure x
anInt _        = empty

aCons :: IntList -> Maybe (IntList, IntList)
aCons (LCons x y) = pure (x, y)
aCons _           = empty

aList :: IntList -> Maybe [IntList]
aList (LCons x xs) = (x:) <$> aList xs
aList LNil         = pure []
aList _            = empty

aListN :: Int -> IntList -> Maybe [IntList]
aListN 0 LNil                 = pure []
aListN n (LCons x xs) | n > 0 = (x:) <$> aListN (n - 1) xs
aListN _ _                    = empty

aMaybeAsNil :: IntList -> (IntList -> Maybe a) -> Maybe (Maybe a)
aMaybeAsNil LNil _ = pure Nothing
aMaybeAsNil p    f = Just <$> f p

aPairAsList :: (Protocol a, Protocol b) => IntList -> Maybe (a, b)
aPairAsList p = aListN 2 p >>= \[a, b] -> (,) <$> fromProto a <*> fromProto b

aVariant :: [(Integer, IntList -> Maybe a)] -> IntList -> Maybe a
aVariant vs (LCons (LInt n) xs) = go vs
  where
    go ((v, h):vs) | v == n    = h xs
                   | otherwise = go vs
    go [] = empty
aVariant _ _                    = empty

instance Protocol IntList where
  toProto = id
  fromProto = Just

instance Protocol Integer where
  toProto = LInt
  fromProto = anInt

instance (Protocol a, Protocol b) => Protocol (a, b) where
  toProto (x, y) = LCons (toProto x) (toProto y)
  fromProto p = do
    (i, j) <- aCons p
    (,) <$> fromProto i <*> fromProto j

instance Protocol a => Protocol [a] where
  toProto (x:xs) = LCons (toProto x) (toProto xs)
  toProto []     = LNil
  fromProto (LCons x xs) = (:) <$> fromProto x <*> fromProto xs
  fromProto LNil = pure []
  fromProto _ = empty

newtype GameId = GameId Integer deriving (Eq, Show)
instance Protocol GameId where
  toProto (GameId x) = toProto x
  fromProto p = GameId <$> fromProto p

data Coord = Coord Integer Integer deriving (Eq, Show)
instance Protocol Coord where
  toProto (Coord x y) = toProto (x, y)
  fromProto p = (\(x, y) -> Coord x y) <$> fromProto p

newtype ShipId = ShipId Integer deriving (Eq, Show)
instance Protocol ShipId where
  toProto (ShipId x) = LInt x
  fromProto p = ShipId <$> anInt p

data Stats = Stats
  { hitpoints :: Integer
  , mana      :: Integer
  , charisma  :: Integer
  , telomeres :: Integer
  }
  deriving (Eq, Show)
instance Protocol Stats where
  toProto Stats{..} = toProto [hitpoints, mana, charisma, telomeres]
  fromProto p = aListN 4 p >>= (\[hitpoints, mana, charisma, telomeres] -> Stats <$> fromProto hitpoints <*> fromProto mana <*> fromProto charisma <*> fromProto telomeres)

data Action
  = Boost ShipId Coord
  | Detonate ShipId
  | Laser ShipId Coord Integer {- energy? -}
  | Mitosis ShipId Stats
  deriving (Eq, Show)
instance Protocol Action where
  toProto (Boost id coord)        = toProto [LInt 0, toProto id, toProto coord]
  toProto (Detonate id)           = toProto [LInt 1, toProto id]
  toProto (Laser id target power) = toProto [LInt 2, toProto id, toProto target, toProto power]
  toProto (Mitosis id stats)      = toProto [LInt 3, toProto id, toProto stats]
  fromProto = aVariant
    [ (0, \p -> aListN 2 p >>= \[id, coord] -> Boost <$> fromProto id <*> fromProto coord)
    , (1, \p -> aListN 1 p >>= \[id] -> Detonate <$> fromProto id)
    , (2, \p -> aListN 3 p >>= \[id, target, power] -> Laser <$> fromProto id <*> fromProto target <*> fromProto power)
    , (3, \p -> aListN 2 p >>= \[id, stats] -> Mitosis <$> fromProto id <*> fromProto stats)
    ]

data Request
  = ReqTime
  | ReqCreate (Maybe Integer) {- tutorial number -}
  | ReqJoin GameId [Integer] {- bonus tokens -}
  | ReqStart GameId (Maybe Stats)
  | ReqAct GameId [Action]
  deriving (Eq, Show)
instance Protocol Request where
  toProto ReqTime              = toProto [LInt 0]
  toProto (ReqCreate mNum)     = toProto $ [LInt 1] ++ maybeToList (toProto <$> mNum)
  toProto (ReqJoin id ts)      = toProto [LInt 2, toProto id, toProto ts]
  toProto (ReqStart id mStats) = toProto [LInt 3, toProto id, fromMaybe LNil $ toProto <$> mStats]
  toProto (ReqAct id acts)     = toProto [LInt 4, toProto id, toProto acts]
  fromProto = aVariant
    [ (0, \_ -> pure ReqTime)
    , (1, \p -> aList p >>= \case
                  [num] -> ReqCreate <$> Just <$> fromProto num
                  []    -> pure $ ReqCreate Nothing
                  _     -> empty)
    , (2, \p -> aListN 2 p >>= \[id, ts] -> ReqJoin <$> fromProto id <*> fromProto ts)
    , (3, \p -> aListN 2 p >>= \[id, mStats] -> ReqStart <$> fromProto id <*> aMaybeAsNil mStats fromProto)
    , (4, \p -> aListN 2 p >>= \[id, acts] -> ReqAct <$> fromProto id <*> fromProto acts)
    ]

data StatsSettings = StatsSettings
  { maxTotal  :: Integer
  , unknown13 :: Integer
  , unknown14 :: Integer
  }
  deriving (Eq, Show)
instance Protocol StatsSettings where
  toProto = error "toProto StatsSettings"
  fromProto p = aListN 3 p >>= \[mt, u13, u14] -> StatsSettings <$> fromProto mt <*> fromProto u13 <*> fromProto u14

data GameInfo = GameInfo
  { unknown1 :: IntList
  , myTeam   :: Integer
  , maxStats :: StatsSettings
  , planet   :: Integer
  , field    :: Integer
  , unknown4 :: IntList
  }
  deriving (Eq, Show)
instance Protocol GameInfo where
  toProto = error "toProto GameInfo"
  fromProto p = do
      [u1, team, ss, u3, u4] <- aListN 5 p
      [planet, field] <- aListN 2 u3
      GameInfo u1 <$> fromProto team <*> fromProto ss <*> fromProto planet <*> fromProto field <*> pure u4

data GameStatus
  = NotStarted
  | Started
  | Finished
  deriving (Eq, Show)
instance Protocol GameStatus where
  toProto NotStarted = LInt 0
  toProto Started    = LInt 1
  toProto Finished   = LInt 2
  fromProto p = anInt p >>= \case
    0 -> pure NotStarted
    1 -> pure Started
    2 -> pure Finished
    _ -> empty

data GameState = GameState
  { gameTick    :: Integer
  , unknown5    :: IntList
  , gameShips :: [(Ship, [SAction])]
  }
  deriving (Eq, Show)
instance Protocol GameState where
  toProto = error "toProto GameState"
  fromProto p = aListN 3 p >>= \[tick, u5, objs] -> GameState <$> fromProto tick <*> pure u5 <*> (fromProto objs >>= mapM aPairAsList)

data Response
  = RespError
  | RespKeys [(Integer {- Team -}, GameId)]
  | RespGame GameStatus GameInfo (Maybe GameState)
  deriving (Eq, Show)
instance Protocol Response where
  toProto = error "toProto Response"
  fromProto = aVariant
    [ (0, \_ -> pure RespError)
    , (1, \p -> parseKeys p <|> parseGame p)
    ]
    where
      parseKeys p = aListN 1 p >>= \[keys] -> RespKeys <$> (fromProto keys >>= mapM aPairAsList)
      parseGame p = aListN 3 p >>= \[status, info, state] -> RespGame <$> fromProto status <*> fromProto info <*> aMaybeAsNil state fromProto

data Ship = Ship
  { shipTeam  :: Integer
  , shipId    :: ShipId
  , shipPos   :: Coord
  , shipVel   :: Coord
  , shipStats :: Stats
  , temp      :: Integer
  , unknown8  :: IntList
  , unknown9  :: IntList
  }
  deriving (Eq, Show)
instance Protocol Ship where
  toProto = error "toProto Ship"
  fromProto p = aListN 8 p >>= \[team, id, pos, vel, stats, temp, u8, u9] -> Ship <$> fromProto team <*> fromProto id <*> fromProto pos <*> fromProto vel <*> fromProto stats <*> fromProto temp <*> pure u8 <*> pure u9

-- Same as Action but without ship id
data SAction
  = SBoost Coord
  | SDetonate Integer Integer {- possibly explosion? -}
  | SLaser Coord Integer {- energy? -} Integer Integer {- ? -}
  | SMitosis Stats
  deriving (Eq, Show)
instance Protocol SAction where
  toProto (SBoost coord)        = toProto [LInt 0, toProto coord]
  toProto (SDetonate u10 u11)   = toProto [LInt 1, toProto u10, toProto u11]
  toProto (SLaser target power u12 u13) = toProto [LInt 2, toProto target, toProto power, toProto u12, toProto u13]
  toProto (SMitosis stats)  = toProto [LInt 3, toProto stats]
  fromProto = aVariant
    [ (0, \p -> aListN 1 p >>= \[coord] -> SBoost <$> fromProto coord)
    , (1, \p -> aListN 2 p >>= \[u10, u11] -> SDetonate <$> fromProto u10 <*> fromProto u11)
    , (2, \p -> aListN 4 p >>= \[target, power, u12, u13] -> SLaser <$> fromProto target <*> fromProto power <*> fromProto u12 <*> fromProto u13)
    , (3, \p -> aListN 1 p >>= \[stats] -> SMitosis <$> fromProto stats)
    ]
