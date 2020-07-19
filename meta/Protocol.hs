{-# LANGUAGE RecordWildCards #-}

module Protocol where

import Alien.FFI
import Control.Monad
import Data.Char
import Data.List
import Control.Monad
import Control.Applicative


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
aListN n (LCons x xs) | n > 0 = (x:) <$> aList xs
aListN _ _                    = empty

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

data Action
  = Boost ShipId Coord
  | Detonate ShipId
  | Laser ShipId Coord Integer {- energy? -}
  | Mitosis ShipId Stats
instance Protocol Action where
  toProto (Boost id coord)        = toProto [LInt 0, toProto id, toProto coord]
  toProto (Detonate id)           = toProto [LInt 1, toProto id]
  toProto (Laser id target power) = toProto [LInt 2, toProto id, toProto target, toProto power]
  toProto (Mitosis id Stats{..})  = toProto [LInt 3, toProto id, toProto hitpoints, toProto mana, toProto charisma, toProto telomeres]
  fromProto = aVariant
    [ (0, \p -> aListN 2 p >>= \[id, coord] -> Boost <$> fromProto id <*> fromProto coord)
    , (1, \p -> aListN 1 p >>= \[id] -> Detonate <$> fromProto id)
    , (2, \p -> aListN 3 p >>= \[id, target, power] -> Laser <$> fromProto id <*> fromProto target <*> fromProto power)
    , (3, \p -> aListN 5 p >>= \[id, hitpoints, mana, charisma, telomeres] -> Mitosis <$> fromProto id <*> (Stats <$> fromProto hitpoints <*> fromProto mana <*> fromProto charisma <*> fromProto telomeres))
    ]

{-
data Requests = DoThis Integer [[Command]] deriving (Show)

instance Show Command where
    show (Detonate i) = show i ++ " detonates"
    show (Move i v) = show i ++ " moves by " ++ show v
    show (Fire i v e) = show i ++ " fires at " ++ show v ++ " " ++ show e
    show (Spawn i hp mn ch tl) = show i ++ " spawns " ++
                                 "HP: " ++ show hp ++ ", MN: " ++ show mn ++ ", CH: " ++ show ch ++ ", TL: " ++ show tl
    show (Unk list) = pprList list

tryList :: IntList -> Maybe [IntList]
tryList LNil = return []
tryList (LCons x xs) = do
    xs' <- tryList xs
    return (x : xs')

tryCommand :: IntList -> Command
tryCommand (LCons (LInt 0) (LCons (LInt i) (LCons (LCons (LInt x) (LInt y)) LNil))) = Move i (x, y)
tryCommand (LCons (LInt 1) (LCons (LInt i) LNil)) = Detonate i
tryCommand (LCons (LInt 2) (LCons (LInt i) (LCons (LCons (LInt x) (LInt y)) (LCons (LInt e) LNil)))) = Fire i (x, y) e
tryCommand (LCons
             (LInt 3)
             (LCons
               (LInt i)
               (LCons
                 (LCons
                   (LInt hp)
                   (LCons
                     (LInt mana)
                     (LCons
                       (LInt ch)
                       (LCons (LInt telo) LNil)
                     )
                   )
                 )
               LNil))) =
    Spawn i hp mana ch telo
tryCommand list = Unk list

tryInterpret (LCons (LInt 4) (LCons (LInt key) list)) = do
    bots <- tryList list
    cmds <- mapM tryList bots
    let cmdint = map (map tryCommand) cmds
    return (DoThis key cmdint)
tryInterpret ls = Nothing

showInterpret ls = case tryInterpret ls of
                       Just r -> show r
                       Nothing -> pprList ls
-}
