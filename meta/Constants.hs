{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-missing-signatures -Wno-type-defaults #-}
module Constants where

import Numeric.Natural
default (Natural)

apOpNum = 0
eqOpNum = 12

idOpNum = 1
trueOpNum = 2
compOpNum = 5
flipOpNum = 6
sOpNum = 7
falseOpNum = 8
negOpNum = 10
nilOpNum = 14
isNilOpNum = 15
divOpNum = 40
mulOpNum = 146
modOpNum = 170
evalOpNum = 174
demOpNum = 341
addOpNum = 365
predOpNum = 401
ltOpNum = 416
succOpNum = 417
bEqOpNum = 448
chooseOpNum = 58336
pairOpNum = 64170
fstOpNum = 64174
sndOpNum = 64171
pointOpNum = 17043521
imageOpNum = 33047056
checkerOpNum = 11184810
pow2OpNum = 68191693600
mapImageOpNum = 68259412260

fromPegovkaOpNum :: String -> Maybe Natural
fromPegovkaOpNum = \case
  "ap" -> Just apOpNum
  "=" -> Just eqOpNum

  "i" -> Just idOpNum
  "t" -> Just trueOpNum
  "b" -> Just compOpNum
  "c" -> Just flipOpNum
  "s" -> Just sOpNum
  "f" -> Just falseOpNum
  "neg" -> Just negOpNum
  "nil" -> Just nilOpNum
  "isnil" -> Just isNilOpNum
  "div" -> Just divOpNum
  "mul" -> Just mulOpNum
  "mod" -> Just modOpNum
  "eval" -> Just evalOpNum
  "dem" -> Just demOpNum
  "add" -> Just addOpNum
  "pred" -> Just predOpNum
  "lt" -> Just ltOpNum
  "succ" -> Just succOpNum
  "eq" -> Just bEqOpNum
  "choose" -> Just chooseOpNum
  "cons" -> Just pairOpNum
  "car" -> Just fstOpNum
  "cdr" -> Just sndOpNum
  "point" -> Just pointOpNum
  "image" -> Just imageOpNum
  "checkerboard" -> Just checkerOpNum
  "pwr2" -> Just pow2OpNum
  "mapImage" -> Just mapImageOpNum

  "galaxy" -> Just 123229502148636

  _ -> Nothing
