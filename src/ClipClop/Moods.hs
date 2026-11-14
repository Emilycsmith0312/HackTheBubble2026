-- | Horse moods: the non-deterministic part of Clip Clop.
--
-- Every time a horse is used in an evaluation there is a chance it changes mood:
--
--   * 'Sulking' - it ignores every evaluation until it is given a treat
--   * 'Lying'   - it silently gives a wrong value until it is interrogated
--   * 'RanAway' - it cannot be evaluated until it is coaxed back with honey
--
-- Two horses used together may also start a feud, after which they refuse to
-- appear in the same evaluation until they are forced to make up.
--
-- This module is pure: the random numbers come from a small generator ('Gen')
-- that the REPL threads through, so a fixed seed always gives the same moods.
module ClipClop.Moods where

import ClipClop.Value (Function (..), Value (..))
import Data.Bits (shiftR, (.&.))
import Data.List (tails)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

data Mood = Sulking | Lying | RanAway
  deriving (Show, Eq, Enum, Bounded)

-- | The moods and feuds of every horse. A horse with no entry is calm.
data Herd = Herd
  { herdMoods :: Map String Mood
  , herdFeuds :: Set (String, String) -- each pair stored once, smallest name first
  } deriving (Show, Eq)

emptyHerd :: Herd
emptyHerd = Herd Map.empty Set.empty

moodOf :: String -> Herd -> Maybe Mood
moodOf n = Map.lookup n . herdMoods

setMood :: String -> Mood -> Herd -> Herd
setMood n m h = h { herdMoods = Map.insert n m (herdMoods h) }

clearMood :: String -> Herd -> Herd
clearMood n h = h { herdMoods = Map.delete n (herdMoods h) }

feudKey :: String -> String -> (String, String)
feudKey a b = (min a b, max a b)

feuding :: String -> String -> Herd -> Bool
feuding a b = Set.member (feudKey a b) . herdFeuds

startFeud, endFeud :: String -> String -> Herd -> Herd
startFeud a b h = h { herdFeuds = Set.insert (feudKey a b) (herdFeuds h) }
endFeud a b h   = h { herdFeuds = Set.delete (feudKey a b) (herdFeuds h) }

-- | Remove every trace of a horse (it died).
forget :: String -> Herd -> Herd
forget n h = Herd (Map.delete n (herdMoods h))
                  (Set.filter (\(a, b) -> a /= n && b /= n) (herdFeuds h))

-- | What a lying horse says instead of the truth. The lie always has the same
-- type as the truth, so it passes type checks and spreads quietly.
lieAbout :: Value -> Value
lieAbout (IntValue n)   = IntValue (n + 1)
lieAbout (FloatValue x) = FloatValue (x + 1)
lieAbout (BoolValue b)  = BoolValue (not b)
lieAbout (CharValue c)  = CharValue (if c == maxBound then pred c else succ c)
lieAbout (StrValue s)
  | reverse s == s      = StrValue (s ++ "neigh") -- palindromes need a different lie
  | otherwise           = StrValue (reverse s)
lieAbout (FunValue f)   = FunValue f { fnLies = True } -- every result it gives is a lie
lieAbout (ListValue xs)
  | reverse xs /= xs    = ListValue (reverse xs)      -- a lying herd lines up backwards
  | otherwise           = ListValue (map lieAbout xs) -- or, if that shows nothing, every member lies
lieAbout v              = v -- nothing to lie about

-- RANDOMNESS --

-- | A linear congruential generator (Knuth's MMIX constants; Int arithmetic wraps).
newtype Gen = Gen Int

mkGen :: Int -> Gen
mkGen seed = snd (roll 2 (Gen seed)) -- one step, so small seeds are mixed up

-- | A number from 0 to n-1, and the next generator.
roll :: Int -> Gen -> (Int, Gen)
roll n (Gen s) = (((s' `shiftR` 33) .&. 0x7fffffff) `mod` n, Gen s')
  where s' = s * 6364136223846793005 + 1442695040888963407

-- | A horse changes mood (and a pair starts a feud) one time in this many.
moodOdds :: Int
moodOdds = 8

-- | Give every calm horse in the list a chance to change mood, and every pair
-- of them that is not already feuding a chance to fall out.
stirUp :: [String] -> Herd -> Gen -> (Herd, Gen)
stirUp names herd0 gen0 = foldl feudStep (foldl moodStep (herd0, gen0) names) pairs
  where
    pairs = [(a, b) | (a : rest) <- tails names, b <- rest]
    moodStep (h, g) n
      | Map.member n (herdMoods h) = (h, g)
      | r == 0                     = (setMood n (toEnum m) h, g2)
      | otherwise                  = (h, g1)
      where (r, g1) = roll moodOdds g
            (m, g2) = roll (fromEnum (maxBound :: Mood) + 1) g1
    feudStep (h, g) (a, b)
      | feuding a b h = (h, g)
      | r == 0        = (startFeud a b h, g1)
      | otherwise     = (h, g1)
      where (r, g1) = roll moodOdds g
