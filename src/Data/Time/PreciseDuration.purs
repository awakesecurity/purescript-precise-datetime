module Data.Time.PreciseDuration
  ( module PreciseDurationType
  , toString
  , nanoseconds, microseconds, milliseconds, seconds, minutes, hours, days, weeks
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
  ) where

import Prelude

import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Decimal (Decimal)
import Data.Decimal as Decimal

import Data.Time.PreciseDuration.Internal (PreciseDuration(..), day, hour, micro, milli, minute, second, week)
import Data.Time.PreciseDuration.Internal (PreciseDuration) as PreciseDurationType

toString :: PreciseDuration -> String
toString = case _ of
  Nanoseconds d -> Decimal.toString d <> "ns"
  Microseconds d -> Decimal.toString d <> "us"
  Milliseconds d -> Decimal.toString d <> "ms"
  Seconds d -> Decimal.toString d <> "s"
  Minutes d -> Decimal.toString d <> "m"
  Hours d -> Decimal.toString d <> "h"
  Days d -> Decimal.toString d <> "d"
  Weeks d -> Decimal.toString d <> "w"

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
  Nanoseconds d  -> d
  Microseconds d -> d * micro
  Milliseconds d -> d * milli
  Seconds d      -> d * second
  Minutes d      -> d * minute
  Hours d        -> d * hour
  Days d         -> d * day
  Weeks d        -> d * week

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
toHours hours@(Hours _) = hours
toHours duration = Hours $ (Decimal.truncated (unPreciseDuration duration)) / hour

toDays :: PreciseDuration -> PreciseDuration
toDays days@(Days _) = days
toDays duration = Days $ (Decimal.truncated (unPreciseDuration duration)) / day

toWeeks :: PreciseDuration -> PreciseDuration
toWeeks weeks@(Weeks _) = weeks
toWeeks duration = Weeks $ (Decimal.truncated (unPreciseDuration duration)) / week
