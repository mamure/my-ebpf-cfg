module Interval where

data Bound
  = NegInf
  | Fin !Integer
  | PosInf
  deriving (Eq, Ord, Show)

data Itv
  = Bot
  | Ival Bound Bound
  deriving (Eq, Show)

mkIval :: Bound -> Bound -> Itv
mkIval lower upper
  | lower > upper = Bot
  | otherwise = Ival lower upper

botItv :: Itv
botItv = Bot

topItv :: Itv
topItv = Ival NegInf PosInf

leqItv :: Itv -> Itv -> Bool
leqItv Bot _ = True
leqItv _ Bot = False
leqItv (Ival a b) (Ival c d) = c <= a && b <= d

joinItv :: Itv -> Itv -> Itv
joinItv Bot value = value
joinItv value Bot = value
joinItv (Ival a b) (Ival c d) =
  Ival (min a c) (max b d)

meetItv :: Itv -> Itv -> Itv
meetItv Bot _ = Bot
meetItv _ Bot = Bot
meetItv (Ival a b) (Ival c d) =
  mkIval (max a c) (min b d)

constItv :: Integer -> Itv
constItv value = Ival (Fin value) (Fin value)

widenItv :: Itv -> Itv -> Itv
widenItv Bot value = value
widenItv value Bot = value
widenItv (Ival oldLower oldUpper) (Ival newLower newUpper) =
  Ival
    (if newLower < oldLower then NegInf else oldLower)
    (if newUpper > oldUpper then PosInf else oldUpper)

narrowItv :: Itv -> Itv -> Itv
narrowItv _ Bot = Bot
narrowItv Bot _ = Bot
narrowItv
  (Ival lower upper)
  (Ival newLower newUpper) =
    mkIval
      (if lower == NegInf then newLower else lower)
      (if upper == PosInf then newUpper else upper)

addItv :: Itv -> Itv -> Itv
addItv Bot _ = Bot
addItv _ Bot = Bot
addItv (Ival leftLower leftUpper) (Ival rightLower rightUpper) =
  Ival
    (addBound leftLower rightLower)
    (addBound leftUpper rightUpper)

subItv :: Itv -> Itv -> Itv
subItv Bot _ = Bot
subItv _ Bot = Bot
subItv (Ival leftLower leftUpper) (Ival rightLower rightUpper) =
  Ival
    (subBound leftLower rightUpper)
    (subBound leftUpper rightLower)

mulItv :: Itv -> Itv -> Itv
mulItv Bot _ = Bot
mulItv _ Bot = Bot
mulItv _ _ = topItv

bitwiseTop :: Itv -> Itv -> Itv
bitwiseTop Bot _ = Bot
bitwiseTop _ Bot = Bot
bitwiseTop _ _ = topItv

shiftTop :: Itv -> Itv -> Itv
shiftTop Bot _ = Bot
shiftTop _ Bot = Bot
shiftTop _ _ = topItv

addBound :: Bound -> Bound -> Bound
addBound NegInf _ = NegInf
addBound _ NegInf = NegInf
addBound PosInf _ = PosInf
addBound _ PosInf = PosInf
addBound (Fin left) (Fin right) = Fin (left + right)

negBound :: Bound -> Bound
negBound NegInf = PosInf
negBound PosInf = NegInf
negBound (Fin value) = Fin (-value)

subBound :: Bound -> Bound -> Bound
subBound left right = addBound left (negBound right)