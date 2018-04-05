module Data.Time.PreciseDuration
  ( module PreciseDurationType
  , toString
  , unwrapPreciseDuration
  , toNanoseconds
  , toMicroseconds
  , toMilliseconds
  , toSeconds
  , toMinutes
  , toHours
  , toDays
  , toWeeks
  ) where

import Prelude (($), (/), (>>>), (<<<), (<>))

import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Decimal (Decimal)
import Data.Decimal as Decimal

import Data.Time.PreciseDuration.Internal (PreciseDuration(..), day, hour, micro, milli, minute, second, toNanosecondsD, week)
import Data.Time.PreciseDuration.Internal (PreciseDuration(..)) as PreciseDurationType

toString :: PreciseDuration -> String
toString =
  case _ of
    Nanoseconds d -> Decimal.toString d <> "ns"
    Microseconds d -> Decimal.toString d <> "us"
    Milliseconds d -> Decimal.toString d <> "ms"
    Seconds d -> Decimal.toString d <> "s"
    Minutes d -> Decimal.toString d <> "m"
    Hours d -> Decimal.toString d <> "h"
    Days d -> Decimal.toString d <> "d"
    Weeks d -> Decimal.toString d <> "w"

unwrapPreciseDuration :: PreciseDuration -> Decimal
unwrapPreciseDuration = case _ of
  Nanoseconds d -> d
  Microseconds d -> d
  Milliseconds d -> d
  Seconds d -> d
  Minutes d -> d
  Hours d -> d
  Days d -> d
  Weeks d -> d

-- Smart constructors
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

-- Conversions
toNanoseconds :: PreciseDuration -> PreciseDuration
toNanoseconds = Nanoseconds <<< toNanosecondsD

-- NB: We don't use 'toNanosecondsBI' in these conversion functions to avoid a
-- roundtrip between the external 'Decimal' and 'BigInt' types. Instead, we will
-- truncate the decimal to maintain the integral nanoseconds invariant.

toMicroseconds :: PreciseDuration -> PreciseDuration
toMicroseconds duration = Microseconds $ (Decimal.truncated (toNanosecondsD duration)) / micro

toMilliseconds :: PreciseDuration -> PreciseDuration
toMilliseconds duration = Milliseconds $ (Decimal.truncated (toNanosecondsD duration)) / milli

toSeconds :: PreciseDuration -> PreciseDuration
toSeconds duration = Seconds $ (Decimal.truncated (toNanosecondsD duration)) / second

toMinutes :: PreciseDuration -> PreciseDuration
toMinutes duration = Minutes $ (Decimal.truncated (toNanosecondsD duration)) / minute

toHours :: PreciseDuration -> PreciseDuration
toHours duration = Hours $ (Decimal.truncated (toNanosecondsD duration)) / hour

toDays :: PreciseDuration -> PreciseDuration
toDays duration = Days $ (Decimal.truncated (toNanosecondsD duration)) / day

toWeeks :: PreciseDuration -> PreciseDuration
toWeeks duration = Weeks $ (Decimal.truncated (toNanosecondsD duration)) / week
