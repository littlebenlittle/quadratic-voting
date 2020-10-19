
module QuadraticVoting.Base where

data Error = Error { errmsg :: String }

data Nat = Zero | Suc Nat
  deriving (Eq, Show, Read)

instance Num Nat where
  Zero  + a = a
  Suc a + b = Suc (a + b)
  Zero  * a  = a
  Suc a * b  = b + a * b
  fromInteger x = if x < 0
                     then error ("cannot parse " ++ show x ++ " as Nat")
                     else intrec x (\n ->  Suc n) Zero

instance Ord Nat where
  Zero    <= a = True
  (Suc a) <= b = a <= Suc (Suc b)

-- integer recursion
intrec :: Integer -> (a -> a) -> a -> a
intrec 0 _ start = start
intrec i fn start
  = if i < 0
      then error "no recursion for integers less than 0"
      else fn (intrec (i-1) fn start)

data Map a b = Map {
  keys :: [a],
  vals :: [b]
} deriving (Show, Read)

newMap :: Map a b
newMap = Map { keys=[], vals=[] }

appendMap :: Map a b -> a -> b -> Map a b
appendMap Map { keys=ks, vals=vs } key val
  = Map { keys = (key:ks), vals = (val:vs) }

mapLookUp :: (Eq a) => Map a b -> a -> b
mapLookUp Map { keys=(k:ks), vals=(v:vs) } key
  = if k == key
       then v
       else mapLookUp Map { keys=ks, vals=vs } key

kvpairs :: Map a b -> [(a,b)]
kvpairs Map { keys=[], vals=[] } = []
kvpairs Map { keys=(k:ks), vals=(v:vs) }
  = (k,v) : kvpairs Map { keys=ks, vals=vs }

mapfilter :: (Eq a) => Map a b -> [a] -> [b]
mapfilter _   []     = []
mapfilter map (k:ks) = (mapLookUp map k) : mapfilter map ks
     
