module Data.Tempo.Random (countToRand, countToRands) where

import Data.Bits

xorwise :: Int -> Int
xorwise x =
  let a = xor (shiftL x 13) x
      b = xor (shiftR a 17) a
  in xor (shiftL b 5) b

countToIntSeed :: RealFrac a => a -> Int
countToIntSeed = xorwise . truncate . (* 536870912) . snd . (properFraction :: (RealFrac a => a -> (Int,a))) . (/ 300)

intSeedToRand :: Fractional a => Int -> a
intSeedToRand = (/ 536870912) . realToFrac . (`mod` 536870912)

-- | countToRand converts a count (eg. measure of elapsed 'time' in a Tempo) to
-- a random value in the range [0,1) by stretching 300 counts over the range of
-- [0,2**29 == 536870912) and then apply a 'xorshift' algorithm. For the latter:
-- cf. George Marsaglia (2003). "Xorshift RNGs". Journal of Statistical Software 8:14.
-- https://www.jstatsoft.org/article/view/v008i14

countToRand :: (RealFrac a, Fractional b) => a -> b
countToRand = intSeedToRand . countToIntSeed

-- | countToRands generates multiple pseudo-random values by converting the provided
-- count as with countToRand and then recursively using the values calculated to
-- generate additional pseudo-random values, as in the 'normal' usage of a pseudo-
-- random number geneator.

countToRands :: (RealFrac a, Fractional b) => a -> Int -> [b]
countToRands t n = countToRands' (countToIntSeed t) n

countToRands' :: Fractional a => Int -> Int -> [a]
countToRands' seed n
  | n <= 0 = []
  | otherwise = (intSeedToRand seed) : (countToRands' (xorwise seed) (n-1))
