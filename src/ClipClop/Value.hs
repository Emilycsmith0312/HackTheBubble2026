module ClipClop.Value where

data Value = IntValue Int | StrValue String | FloatValue Double | BoolValue Bool | CharValue Char | UnitValue | ErrValue String
        deriving (Eq)

instance Show Value where
  show (IntValue x) = showBinInt x
  show (FloatValue x) = showBinFloat x
  show (StrValue x) = x
  show (CharValue c) = [c]
  show UnitValue = "()"
  show (BoolValue True) = "neighneigh"
  show (BoolValue False) = "neigh"
  show (ErrValue x) = x

-- BINARY NUMBER FORMAT --
-- Numbers are written in binary, with each digit spelled out: 0 is "cl0p", 1 is "cl1p".
-- e.g. 5 = cl1pcl0pcl1p, -2 = -cl1pcl0p, 2.5 = cl1pcl0p.cl1p

bitWord :: Bool -> String
bitWord False = "cl0p"
bitWord True  = "cl1p"

-- | Binary digits of a non-negative whole number, most significant first.
natBits :: Integer -> [Bool]
natBits 0 = [False]
natBits n = reverse (go n)
  where go 0 = []
        go k = odd k : go (k `div` 2)

showBinInt :: Int -> String
showBinInt x
  | x < 0     = '-' : showBinInt' (negate (toInteger x))
  | otherwise = showBinInt' (toInteger x)
  where showBinInt' = concatMap bitWord . natBits

-- | Floats are binary internally, so the fractional part always terminates.
showBinFloat :: Double -> String
showBinFloat x
  | isNaN x       = "NaN"
  | isInfinite x  = if x > 0 then "Infinity" else "-Infinity"
  | x < 0         = '-' : showBinFloat (negate x)
  | otherwise     = concatMap bitWord (natBits whole) ++ "." ++ concatMap bitWord fracBits
  where
    r        = toRational x
    whole    = floor r :: Integer
    fracBits = case go (r - fromInteger whole) of
                 [] -> [False]  -- always show at least one fractional digit
                 bs -> bs
    go 0 = []
    go f = let f2 = f * 2 in (f2 >= 1) : go (if f2 >= 1 then f2 - 1 else f2)

-- | Value of a list of binary digits (most significant first).
bitsToInt :: [Bool] -> Integer
bitsToInt = foldl (\acc b -> acc * 2 + (if b then 1 else 0)) 0

-- | Value of the binary digits after the point.
bitsToFrac :: [Bool] -> Rational
bitsToFrac = foldr (\b acc -> ((if b then 1 else 0) + acc) / 2) 0


-- ADD SUBTRACT MULTIPLY DIVIDE --

addValues                       :: Value -> Value -> Value
addValues (ErrValue x) _                = (ErrValue x)
addValues _ (ErrValue x)                = (ErrValue x)
addValues (IntValue x) (IntValue y)     = (IntValue (x+y))
addValues (FloatValue x) (FloatValue y) = (FloatValue (x+y))
addValues (FloatValue x) (IntValue y)   = (FloatValue (x+(fromIntegral(y))))
addValues (IntValue x) (FloatValue y)   = (FloatValue (y+(fromIntegral(x))))
addValues _ _                           = (ErrValue "Invalid value type for: addition")

subValues                       :: Value -> Value -> Value
subValues (ErrValue x) _                = (ErrValue x)
subValues _ (ErrValue x)                = (ErrValue x)
subValues (IntValue x) (IntValue y)     = (IntValue (x-y))
subValues (FloatValue x) (FloatValue y) = (FloatValue (x-y))
subValues (FloatValue x) (IntValue y)   = (FloatValue (x-(fromIntegral(y))))
subValues (IntValue x) (FloatValue y)   = (FloatValue (y-(fromIntegral(x))))
subValues _ _                           = (ErrValue "Invalid value type for: subtraction")


mulValues                       :: Value -> Value -> Value
mulValues (ErrValue x) _                = (ErrValue x)
mulValues _ (ErrValue x)                = (ErrValue x)
mulValues (IntValue x) (IntValue y)     = (IntValue (x*y))
mulValues (FloatValue x) (FloatValue y) = (FloatValue (x*y))
mulValues (FloatValue x) (IntValue y)   = (FloatValue (x*(fromIntegral(y))))
mulValues (IntValue x) (FloatValue y)   = (FloatValue (y*(fromIntegral(x))))
mulValues _ _                           = (ErrValue "Invalid value type for: multiplication")

divValues                       :: Value -> Value -> Value 
divValues _ (IntValue 0)                = (ErrValue "cannot divide by zero.")
divValues _ (FloatValue 0)              = (ErrValue "cannot divide by zero.")
divValues (IntValue x) (IntValue y)     = if x `mod` y == 0 then (IntValue (x `div` y)) else divValues (FloatValue (fromIntegral(x))) (FloatValue (fromIntegral(y)))
divValues (FloatValue x) (FloatValue y) = (FloatValue (x/y))
divValues (FloatValue x) (IntValue y)   = (FloatValue (x/(fromIntegral(y))))
divValues (IntValue x) (FloatValue y)   = (FloatValue ((fromIntegral(x))/y))
divValues (ErrValue x) _                = (ErrValue x)
divValues _ (ErrValue x)                = (ErrValue x)
divValues _ _                           = (ErrValue "Invalid value type for: division")

-- POWER MODULUS ABSOLUTE --

powValues                       :: Value -> Value -> Value
powValues (ErrValue x) _                = (ErrValue x)
powValues _ (ErrValue x)                = (ErrValue x)
powValues (IntValue x) (IntValue y)     | y > 0       = (IntValue (x^y))  
                                        | y < 0       = (FloatValue ((fromIntegral(x))^(fromIntegral(y))))
                                        | otherwise   = (IntValue 1)
powValues (FloatValue x) (IntValue y)   | y == 0      = (IntValue 1)
                                        | otherwise   = (FloatValue (x^(fromIntegral(y))))
powValues _ _                           = (ErrValue "Invalid value type for: power of")

modValues                       :: Value -> Value -> Value
modValues (ErrValue x) _                = (ErrValue x)
modValues _ (ErrValue x)                = (ErrValue x)
modValues (IntValue x) (IntValue y)     = (IntValue (x `mod` y))
modValues _ _                           = (ErrValue "Invalid value type for: modulus")

absValues                       :: Value -> Value
absValues (ErrValue x)                  = (ErrValue x)
absValues (IntValue x)                  = (IntValue (abs x))
absValues (FloatValue x)                = (FloatValue (abs x))
absValues _                             = (ErrValue "Invalid value type for: absolute value")

-- STRING CONCATINATION --

concatValues                    :: Value -> Value -> Value
concatValues (ErrValue x) _             = (ErrValue x)
concatValues _ (ErrValue x)             = (ErrValue x)
concatValues (StrValue x) (StrValue y)  = StrValue (x ++ y)
concatValues _ _ = (ErrValue "Invalid value type for: string concatination")

-- BOOLEAN ARGUMENTS -- 

andValues                       :: Value -> Value -> Value
andValues (ErrValue x) _                = (ErrValue x) 
andValues _ (ErrValue x)                = (ErrValue x)
andValues (BoolValue x) (BoolValue y)   = BoolValue (x && y)
andValues _ _                           = (ErrValue "Invalid value type for: boolean AND")

nandValues                      :: Value -> Value -> Value
nandValues (ErrValue x) _               = (ErrValue x)
nandValues _ (ErrValue x)               = (ErrValue x)
nandValues x y                          = notValues (andValues x y)

orValues                        :: Value -> Value -> Value
orValues (ErrValue x) _                = (ErrValue x) 
orValues _ (ErrValue x)                = (ErrValue x)
orValues (BoolValue x) (BoolValue y)   = BoolValue (x || y)
orValues _ _                           = (ErrValue "Invalid value type for: boolean OR")

xorValues                        :: Value -> Value -> Value
xorValues (ErrValue x) _                = (ErrValue x) 
xorValues _ (ErrValue x)                = (ErrValue x)
xorValues (BoolValue x) (BoolValue y)   = BoolValue ((x || y) && not (x && y))
xorValues _ _                           = (ErrValue "Invalid value type for: boolean XOR")

norValues                       :: Value -> Value -> Value
norValues (ErrValue x) _                = (ErrValue x) 
norValues _ (ErrValue x)                = (ErrValue x)
norValues x y                           = notValues (orValues x y)

impValues                       :: Value -> Value -> Value
impValues (ErrValue x) _                = (ErrValue x) 
impValues _ (ErrValue x)                = (ErrValue x)
impValues (BoolValue x) (BoolValue y)   = BoolValue ((not x) || y)
impValues _ _                           = (ErrValue "Invalid value type for: boolean IMPLIES")

-- FIX THIS 
eqValues                        :: Value -> Value -> Value
eqValues (ErrValue x) _                 = (ErrValue x)
eqValues _ (ErrValue x)                 = (ErrValue x)
eqValues _ _                            = (ErrValue "Invalid value type for: boolean EQUALS")

notValues                       :: Value -> Value
notValues (ErrValue x)                  = (ErrValue x)
notValues (BoolValue x)                 = BoolValue (not x)
notValues _                             = (ErrValue "Invalid value type for: boolean NOT")


