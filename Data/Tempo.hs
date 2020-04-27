{-# LANGUAGE DeriveGeneric #-}
module Data.Tempo where

import Data.Time
import GHC.Generics

-- | Musical tempo is represented as a data structure with three orthogonal components.
-- (Generic instances are derived in order to allow later generation of instances for
-- Aeson classes.)

data Tempo = Tempo {
  freq :: Rational, -- frequency of cycles/beats, ie. cycles/beats per second
  time :: UTCTime, -- a time at which a number of "elapsed" cycles/beats will be indicated
  count :: Rational -- the number of "elapsed" cycles/beats at the time indicated
  } deriving (Eq,Generic,Show)


-- | The 'origin' of a Tempo is the time at which the number of elapsed cycles/beats
-- would have been 0.

origin :: Tempo -> UTCTime
origin x = addUTCTime (realToFrac $ count x * (-1) / freq x) (time x)


-- | Given a Tempo and a clock time (UTCTime), timeToCount tells us how many cycles/beats
-- have elapsed at that time.

timeToCount :: Tempo -> UTCTime -> Rational
timeToCount x t = (realToFrac $ diffUTCTime t $ time x) * freq x + count x

-- | Given a Tempo and a count of elapsed cycles/beats, countToTime tells us when that "beat"
-- will (or would have) take(n) place.

countToTime :: Tempo -> Rational -> UTCTime
countToTime x c = addUTCTime  (realToFrac $ c / freq x) (origin x)


-- | Provided a new frequency and a pivot time, changeTempo modifies a given Tempo as if
-- the frequency changed at the pivot time, with the count continuing to increase 'monotonically'

changeTempo :: Rational -> UTCTime -> Tempo -> Tempo
changeTempo f t x = Tempo {
  freq = f,
  time = t,
  count = timeToCount x t
  }

-- | For convenience, changeTempoNow is an IO action that changes the frequency of the tempo
-- 'now', ie. at the time returned by a call to getCurrentTime embedded in the action.

changeTempoNow :: Rational -> Tempo -> IO Tempo
changeTempoNow f x = do
  t <- getCurrentTime
  return $ changeTempo f t x

-- | Given a tempo, a window defined by two UTCTime-s, a metre (cycles of cycles), and an offset
-- within that metre, findBeats returns all occurrences of the defined metric position within the window.
-- The window is inclusive at the lower limit, and exclusive at the upper limit (so answers can
-- occur exactly at the lower limit but not at the upper limit).

findBeats :: Tempo -> UTCTime -> UTCTime -> Rational -> Rational -> [Rational]
findBeats tempo lowerLimitUtc upperLimitUtc metre offset =
  let lowerLimitCycles = timeToCount tempo lowerLimitUtc
      upperLimitCycles = timeToCount tempo upperLimitUtc
  in findBeats' metre offset lowerLimitCycles upperLimitCycles

-- | Given a metre and offset (eg. 2 and 0.5 to represent half-way through the first cycle
-- of a metre lasting 2 cycles), and lower and upper limits in elapsed cycles, findBeats'
-- returns all positions that match the given offset and metre and are greater than or equal
-- to the lower limit, and lower than the upper limit.

findBeats' :: Rational -> Rational -> Rational -> Rational -> [Rational]
findBeats' metre offset lowerLimit upperLimit
  | nextBeat metre offset lowerLimit >= upperLimit = []
  | otherwise = nextBeat metre offset lowerLimit : findBeats' metre offset (lowerLimit+metre) upperLimit

-- | Given a metre, offset, and a lower limit in elapsed cycles, nextBeat returns the next
-- position in cycles that matches the given offset and metre, and is greater than
-- or equal to the lower limit.

nextBeat :: Rational -> Rational -> Rational -> Rational
nextBeat metre offset lowerLimit
  | metre == 0 = error "you can't have a metre of 0!!!"
  | otherwise =
  let fract x = x - realToFrac (floor x :: Integer) -- for convenience
      lowerLimitInMetre = lowerLimit/metre -- lower limit expressed in multiples of the metre (cycles of cycles)
      offsetInMetre = fract $ offset/metre -- offset expressed in multiples of the metre
      -- the answer occurs either in this instance of the metre, or the next...
      nextBeatInMetre | offsetInMetre >= (fract lowerLimitInMetre) = (realToFrac (floor lowerLimitInMetre :: Integer)) + offsetInMetre
                      | otherwise = (realToFrac (ceiling lowerLimitInMetre :: Integer)) + offsetInMetre
  in nextBeatInMetre*metre -- translate answer in terms of meter back to cycles
