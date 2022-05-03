module Data.Time.PreciseDuration
  ( PreciseDuration
  , toString
  , negatePreciseDuration
  , nanoseconds
  , microseconds
  , milliseconds
  , seconds
  , minutes
  , hours
  , days
  , weeks
  , unsafeNanoseconds
  , unPreciseDuration
  , toNanoseconds
  , toMicroseconds
  , toMilliseconds
  , toSeconds
  , toMinutes
  , toHours
  , toDays
  , toWeeks
  , toDecimalLossy
  , nano
  , micro
  , milli
  , second
  , minute
  , hour
  , day
  , week
  ) where

import Prelude

import Data.Decimal (Decimal)
import Data.Decimal as Decimal
import Data.PreciseDateTime.Internal (dropWhileEnd)

data PreciseDuration
  = Nanoseconds Decimal
  -- Nanoseconds must be integral, see smart constructor in Data.Time.PreciseDuration.
  | Microseconds Decimal
  | Milliseconds Decimal
  | Seconds Decimal
  | Minutes Decimal
  | Hours Decimal
  | Days Decimal
  | Weeks Decimal

instance eqPreciseDuration :: Eq PreciseDuration where
  eq x y = compare x y == EQ

instance ordPreciseDuration :: Ord PreciseDuration where
  compare (Nanoseconds x) (Nanoseconds y) = compare x y
  compare x y = compare (toNanoseconds x) (toNanoseconds y)

instance showPreciseDuration :: Show PreciseDuration where
  show (Nanoseconds d) = "(Nanoseconds " <> show d <> ")"
  show (Microseconds d) = "(Microseconds " <> show d <> ")"
  show (Milliseconds d) = "(Milliseconds " <> show d <> ")"
  show (Seconds d) = "(Seconds " <> show d <> ")"
  show (Minutes d) = "(Minutes " <> show d <> ")"
  show (Hours d) = "(Hours " <> show d <> ")"
  show (Days d) = "(Days " <> show d <> ")"
  show (Weeks d) = "(Weeks " <> show d <> ")"

toString :: PreciseDuration -> String
toString = case _ of
  Nanoseconds d -> format d <> "ns"
  Microseconds d -> format d <> "us"
  Milliseconds d -> format d <> "ms"
  Seconds d -> format d <> "s"
  Minutes d -> format d <> "m"
  Hours d -> format d <> "h"
  Days d -> format d <> "d"
  Weeks d -> format d <> "w"

  where

  format :: Decimal -> String
  format = trim <<< Decimal.toFixed 20

  trim :: String -> String
  trim = dropWhileEnd (_ == '.') <<< dropWhileEnd (_ == '0')

negatePreciseDuration :: PreciseDuration -> PreciseDuration
negatePreciseDuration = case _ of
  Nanoseconds x -> Nanoseconds (negate x)
  Microseconds x -> Microseconds (negate x)
  Milliseconds x -> Milliseconds (negate x)
  Seconds x -> Seconds (negate x)
  Minutes x -> Minutes (negate x)
  Hours x -> Hours (negate x)
  Days x -> Days (negate x)
  Weeks x -> Weeks (negate x)

-- Smart constructors
unsafeNanoseconds :: Decimal -> PreciseDuration
unsafeNanoseconds = Nanoseconds

-- Nanoseconds must be integral.
nanoseconds :: Int -> PreciseDuration
nanoseconds = Decimal.fromInt >>> Nanoseconds

microseconds :: Decimal -> PreciseDuration
microseconds = Microseconds

milliseconds :: Decimal -> PreciseDuration
milliseconds = Milliseconds

seconds :: Decimal -> PreciseDuration
seconds = Seconds

minutes :: Decimal -> PreciseDuration
minutes = Minutes

hours :: Decimal -> PreciseDuration
hours = Hours

days :: Decimal -> PreciseDuration
days = Days

weeks :: Decimal -> PreciseDuration
weeks = Weeks

unPreciseDuration :: PreciseDuration -> Decimal
unPreciseDuration = case _ of
  Nanoseconds d -> d
  Microseconds d -> d * micro
  Milliseconds d -> d * milli
  Seconds d -> d * second
  Minutes d -> d * minute
  Hours d -> d * hour
  Days d -> d * day
  Weeks d -> d * week

-- Conversions
toNanoseconds :: PreciseDuration -> PreciseDuration
toNanoseconds ns@(Nanoseconds _) = ns
toNanoseconds duration = Nanoseconds (unPreciseDuration duration)

toMicroseconds :: PreciseDuration -> PreciseDuration
toMicroseconds us@(Microseconds _) = us
toMicroseconds duration = Microseconds $ (Decimal.truncated (unPreciseDuration duration)) / micro

toMilliseconds :: PreciseDuration -> PreciseDuration
toMilliseconds ms@(Milliseconds _) = ms
toMilliseconds duration = Milliseconds $ (Decimal.truncated (unPreciseDuration duration)) / milli

toSeconds :: PreciseDuration -> PreciseDuration
toSeconds s@(Seconds _) = s
toSeconds duration = Seconds $ (Decimal.truncated (unPreciseDuration duration)) / second

toMinutes :: PreciseDuration -> PreciseDuration
toMinutes mins@(Minutes _) = mins
toMinutes duration = Minutes $ (Decimal.truncated (unPreciseDuration duration)) / minute

toHours :: PreciseDuration -> PreciseDuration
toHours h@(Hours _) = h
toHours duration = Hours $ (Decimal.truncated (unPreciseDuration duration)) / hour

toDays :: PreciseDuration -> PreciseDuration
toDays d@(Days _) = d
toDays duration = Days $ (Decimal.truncated (unPreciseDuration duration)) / day

toWeeks :: PreciseDuration -> PreciseDuration
toWeeks w@(Weeks _) = w
toWeeks duration = Weeks $ (Decimal.truncated (unPreciseDuration duration)) / week

toDecimalLossy :: PreciseDuration -> Decimal
toDecimalLossy = case _ of
  Nanoseconds d -> d
  Microseconds d -> d
  Milliseconds d -> d
  Seconds d -> d
  Minutes d -> d
  Hours d -> d
  Days d -> d
  Weeks d -> d

-- Each duration in nanoseconds
nano = Decimal.fromInt 1 :: Decimal
micro = (nano * Decimal.fromInt 1000) :: Decimal
milli = (micro * Decimal.fromInt 1000) :: Decimal
second = (milli * Decimal.fromInt 1000) :: Decimal
minute = (second * Decimal.fromInt 60) :: Decimal
hour = (minute * Decimal.fromInt 60) :: Decimal
day = (hour * Decimal.fromInt 24) :: Decimal
week = (day * Decimal.fromInt 7) :: Decimal
