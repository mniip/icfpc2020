{-# LANGUAGE MagicHash #-}
import Text.Read
import GHC.Integer.Logarithms
import GHC.Exts
import Data.List

translate :: String -> String
translate str | Just n <- (readMaybe str :: Maybe Integer)
              = if n >= 0
                then "BAtom $ Num " ++ show n
                else "BAtom $ Num (" ++ show n ++ ")"
              | ('[':x) <- str
              , [(n, "]")] <- (reads x :: [(Integer,String)])
              = if n >= 0
                then "BAtom $ Bits " ++ show n
                else "BAtom $ Bits (" ++ show n ++ ")"
              | ('x':rest) <- str
              , Just n <- (readMaybe rest :: Maybe Integer)
              , n >= 0
              = "BVar " ++ show n
              | str == "("            = "BlistOpen"
              | str == ")"            = "BListClose"
              | str == ","            = "BListComma"
              | str == ":1678847"     = "BUnknownOp 5 5 1678847"
              | str == ":33185746"    = "BUnknownOp 5 5 33185746"
              | str == ":33554448"    = "BUnknownOp 6 6 33554448"
              | str == ":67108929"    = "BUnknownOp 6 6 67108929"
              | str == "="            = "BEq"
              | str == "add"          = "BAtom $ Comb Add"
              | str == "aliens"       = "BUnknown 7 7 375993187999496"
              | str == "ap"           = "BAp"
              | str == "b"            = "BAtom $ Comb Compose"
              | str == "c"            = "BAtom $ Comb Flip"
              | str == "car"          = "BAtom $ Comb Fst"
              | str == "cdr"          = "BAtom $ Comb Snd"
              | str == "checkerboard" = "BAtom $ Comb MkChecker"
              | str == "cons"         = "BAtom $ Comb Pair"
              | str == "dec"          = "BAtom $ Comb Pred"
              | str == "dem"          = "BAtom $ Comb Dem"
              | str == "div"          = "BAtom $ Comb Div"
              | str == "draw"         = "BAtom $ Comb MkImage"
              | str == "eq"           = "BAtom $ Comb EqBool"
              | str == "f"            = "BAtom $ Comb CFalse"
              | str == "humans"       = "BUnknown 7 7 39555634235924"
              | str == "i"            = "BAtom $ Comb Id"
              | str == "if0"          = "BAtom $ Comb Choose"
              | str == "ifzero"       = "BAtom $ Comb Choose" -- same??
              | str == "inc"          = "BAtom $ Comb Succ"
              | str == "interact"     = "BUnknownOp 5 5 33053392"
              | str == "isnil"        = "BAtom $ Comb IsNil"
              | str == "lt"           = "BAtom $ Comb Lt"
              | str == "mod"          = "BAtom $ Comb Mod"
              | str == "modem"        = "BUnknownOp 5 5 7110656"
              | str == "mul"          = "BAtom $ Comb Mul"
              | str == "multipledraw" = "BAtom $ Comb MapImage"
              | str == "neg"          = "BAtom $ Comb Neg"
              | str == "nil"          = "BAtom $ Comb Nil"
              | str == "pwr2"         = "BAtom $ Comb Pow2"
              | str == "s"            = "BAtom $ Comb S"
              | str == "send"         = "BAtom $ Comb Eval"
              | str == "t"            = "BAtom $ Comb CTrue"
              | str == "vec"          = "BAtom $ Comb Point"
              | str == "galaxy"       = "BUnknown 7 7 1338" -- reconsider this
              | (':':xs) <- str
              , Just n <- (readMaybe xs :: Maybe Integer)
              , n >= 0 = case n of
                  0    -> "BAp"
                  1    -> "BAtom $ Comb Id"
                  2    -> "BAtom $ Comb CTrue"
                  5    -> "BAtom $ Comb Compose"
                  6    -> "BAtom $ Comb Flip"
                  7    -> "BAtom $ Comb S"
                  8    -> "BAtom $ Comb CFalse"
                  10   -> "BAtom $ Comb Neg"
                  12   -> "BEq"
                  14   -> "BAtom $ Comb Nil"
                  15   -> "BAtom $ Comb IsNil"
                  40   -> "BAtom $ Comb Div"
                  146  -> "BAtom $ Comb Mul"
                  170  -> "BAtom $ Comb Mod"
                  174  -> "BAtom $ Comb Eval"
                  341  -> "BAtom $ Comb Dem"
                  365  -> "BAtom $ Comb Add"
                  401  -> "BAtom $ Comb Pred"
                  416  -> "BAtom $ Comb Lt"
                  417  -> "BAtom $ Comb Succ"
                  448  -> "BAtom $ Comb EqBool"
                  58336 -> "BAtom $ Comb Choose"
                  64170 -> "BAtom $ Comb Pair"
                  64174 -> "BAtom $ Comb Fst"
                  64171 -> "BAtom $ Comb Snd"
                  17043521 -> "BAtom $ Comb Point"
                  33047056 -> "BAtom $ Comb MkImage"
                  11184810 -> "BAtom $ Comb MkChecker"
                  68191693600 -> "BAtom $ Comb Pow2"
                  68259412260 -> "BAtom $ Comb MapImage"
                  _ -> "BUnknownOp " ++ side ++ " " ++ side ++ " " ++ show n
                    where
                      lg = fromIntegral $ I# (integerLog2# n) + 1
                      wholeSqrt = ceiling $ sqrt lg
                      side = show $ wholeSqrt

-- usage: translate.exe < galaxy.txt > blavla.hs, then copypaste it to Block.hs for example
main = interact $ (++"]") . ("long_list = [" ++ ) . concat . intersperse ", \n  " . map ((++ "]") . concat . ("[":) . intersperse ", " . map translate . words) . lines



{-
translate str | Just n <- (readMaybe str :: Maybe Integer)
              = if n >= 0
                then "BNum " ++ show n
                else "BNum (" ++ show n ++ ")"
              | ('[':x) <- str
              , [(n, "]")] <- (reads x :: [(Integer,String)])
              = if n >= 0
                then "BLineNum " ++ show n
                else "BLineNum (" ++ show n ++ ")"
              | (':':xs) <- str
              , Just n <- (readMaybe xs :: Maybe Integer)
              , n >= 0 = case n of
                  0    -> "BAp"
                  1    -> "BAtom $ Comb Id"
                  2    -> "BAtom $ Comb CTrue"
                  5    -> "BAtom $ Comb Compose"
                  6    -> "BAtom $ Comb Flip"
                  7    -> "BAtom $ Comb S"
                  8    -> "BAtom $ Comb CFalse"
                  10   -> "BAtom $ Comb Neg"
                  12   -> "BEq"
                  14   -> "BAtom $ Comb Nil"
                  15   -> "BAtom $ Comb IsNil"
                  40   -> "BAtom $ Comb Div"
                  146  -> "BAtom $ Comb Mul"
                  170  -> "BAtom $ Comb Mod"
                  174  -> "BAtom $ Comb Eval"
                  341  -> "BAtom $ Comb Dem"
                  365  -> "BAtom $ Comb Add"
                  401  -> "BAtom $ Comb Pred"
                  416  -> "BAtom $ Comb Lt"
                  417  -> "BAtom $ Comb Succ"
                  448  -> "BAtom $ Comb EqBool"
                  58336 -> "BAtom $ Comb Choose"
                  64170 -> "BAtom $ Comb Pair"
                  64174 -> "BAtom $ Comb Fst"
                  64171 -> "BAtom $ Comb Snd"
                  17043521 -> "BAtom $ Comb Point"
                  33047056 -> "BAtom $ Comb MkImage"
                  11184810 -> "BAtom $ Comb MkChecker"
                  68191693600 -> "BAtom $ Comb Pow2"
                  68259412260 -> "BAtom $ Comb MapImage"
                  _ -> "BUnknowOp " ++ side ++ " " ++ side ++ " " ++ show n
                    where
                      lg = I# (integerLog2# n)
                      side = show $ ceiling $ sqrt lg
              | ('x':rest) <- str
              , Just n <- (readMaybe str :: Maybe Integer)
              , n >= 0
              = "BVar " ++ show n
              | str == "="    = "BEq"
              | str == "add"  = "BAtom $ Comb Add"
              | str == "ap"   = "BAp"
              | str == "b"    = "BComp"
              | str == "c"    = "BFlip"
              | str == "car"  = "BFst"
              | str == "cdr"  = "BSnd"
              | str == "cons" = "BPair"
              | str == "dec"  = "BPred"
              | str == "dem"  = "BDem"
              | str == "div"  = "BDiv"
              | str == "eq"   = "BBEq"
              | str == "f"    = "BFalse"
              | str == "i"    = "BId"
              | str == "inc"  = "BSucc"
              | str == "lt"   = "BLt"
              | str == "mod"  = "BMod"
              | str == "mul"  = "BMul"
              | str == "neg"  = "BNeg"
              | str == "pwr2" = "BPow2"
              | str == "s"    = "BS"
              | str == "send" = "BSend"
              | str == "t"    = "BTrue"
              | str == "("    = "BListOpen"
              | str == ","    = "BListComma"
              | str == ")"    = "BListClose"
              | otherwise = str
-}



{-
...                            message1-decoded.txt

(                              message30-decoded.txt
)                              message30-decoded.txt
)]                             message35-decoded.txt
,                              message30-decoded.txt
:1678847                       message36-decoded.txt
:33053392                      message40-decoded.txt
:33185746                      message38-decoded.txt
:33554448                      message40-decoded.txt
:67108929                      message41-decoded.txt
=                              message10-decoded.txt
?                              message40-decoded.txt
add                            message17-decoded.txt
aliens                         message15-decoded.txt
ap                             message10-decoded.txt
b                              message20-decoded.txt
c                              message19-decoded.txt
car                            message26-decoded.txt
cdr                            message27-decoded.txt
checkerboard                   message33-decoded.txt
cons                           message25-decoded.txt
dec                            message17-decoded.txt
dem                            message14-decoded.txt
div                            message10-decoded.txt
draw                           message32-decoded.txt
eq                             message11-decoded.txt
f                              message11-decoded.txt
humans                         message15-decoded.txt
i                              message21-decoded.txt
if0                            message37-decoded.txt
ifzero                         message38-decoded.txt
inc                            message17-decoded.txt
interact                       message38-decoded.txt
isnil                          message29-decoded.txt
lt                             message12-decoded.txt
mod                            message13-decoded.txt
modem                          message38-decoded.txt
mul                            message17-decoded.txt
multipledraw                   message34-decoded.txt
neg                            message16-decoded.txt
nil                            message28-decoded.txt
nil]                           message35-decoded.txt
pwr2                           message23-decoded.txt
s                              message18-decoded.txt
send                           message15-decoded.txt
t                              message11-decoded.txt
vec                            message31-decoded.txt
x0                             message10-decoded.txt
x1                             message15-decoded.txt
x16                            message39-decoded.txt
x17                            message39-decoded.txt
x18                            message39-decoded.txt
x19                            message39-decoded.txt
x2                             message18-decoded.txt
x3                             message38-decoded.txt
x4                             message38-decoded.txt
x5                             message30-decoded.txt
x6                             message39-decoded.txt
x64                            message39-decoded.txt
x65                            message39-decoded.txt
x66                            message39-decoded.txt
x67                            message39-decoded.txt
|picture1|                     message32-decoded.txt
|picture2|                     message32-decoded.txt
|picture3|                     message32-decoded.txt
|picture4|                     message32-decoded.txt
|picture5|                     message32-decoded.txt
|picture6|                     message32-decoded.txt
~~~~~                          message15-decoded.txt
-}
