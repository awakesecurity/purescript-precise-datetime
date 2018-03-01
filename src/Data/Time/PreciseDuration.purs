module Data.Time.PreciseDuration
  ( module PreciseDurationType
  , toString
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

import Prelude (($), (/), (<<<), (<>))

import Data.BigInt as BigInt
import Data.Decimal (Decimal)
import Data.Decimal as Decimal

import Data.Time.PreciseDuration.Internal
import Data.Time.PreciseDuration.Internal (PreciseDuration(..)) as PreciseDurationType

toString :: PreciseDuration -> String
toString =
  case _ of
    Nanoseconds d -> BigInt.toString d <> "ns"
    Microseconds d -> Decimal.toString d <> "us"
    Milliseconds d -> Decimal.toString d <> "ms"
    Seconds d -> Decimal.toString d <> "s"
    Minutes d -> Decimal.toString d <> "m"
    Hours d -> Decimal.toString d <> "h"
    Days d -> Decimal.toString d <> "d"
    Weeks d -> Decimal.toString d <> "w"

unPreciseDuration :: PreciseDuration -> Decimal
unPreciseDuration = case _ of
  Nanoseconds d -> bigIntToDecimal d
  Microseconds d -> d
  Milliseconds d -> d
  Seconds d -> d
  Minutes d -> d
  Hours d -> d
  Days d -> d
  Weeks d -> d

toNanoseconds :: PreciseDuration -> PreciseDuration
toNanoseconds = Nanoseconds <<< toNanosecondsBI

toMicroseconds :: PreciseDuration -> PreciseDuration
toMicroseconds duration = Microseconds $ (toNanosecondsD duration) / micro

toMilliseconds :: PreciseDuration -> PreciseDuration
toMilliseconds duration = Milliseconds $ (toNanosecondsD duration) / milli

toSeconds :: PreciseDuration -> PreciseDuration
toSeconds duration = Seconds $ (toNanosecondsD duration) / second

toMinutes :: PreciseDuration -> PreciseDuration
toMinutes duration = Minutes $ (toNanosecondsD duration) / minute

toHours :: PreciseDuration -> PreciseDuration
toHours duration = Hours $ (toNanosecondsD duration) / hour

toDays :: PreciseDuration -> PreciseDuration
toDays duration = Days $ (toNanosecondsD duration) / day

toWeeks :: PreciseDuration -> PreciseDuration
toWeeks duration = Weeks $ (toNanosecondsD duration) / week
