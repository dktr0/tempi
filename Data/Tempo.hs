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
