module Data.Time.PreciseDuration where

import Prelude

import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Decimal (Decimal)
import Data.Decimal as Decimal
import Data.Maybe (fromMaybe)

data PreciseDuration
  = Nanoseconds BigInt -- this is BigInt to prevent fractional nanoseconds
  | Microseconds Decimal
  | Milliseconds Decimal
  | Seconds Decimal
  | Minutes Decimal
  | Hours Decimal
  | Days Decimal
  | Weeks Decimal

derive instance eqPreciseDuration :: Eq PreciseDuration
derive instance ordPreciseDuration :: Ord PreciseDuration

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

-- Each duration in nanoseconds
nano = Decimal.fromInt 1 :: Decimal
micro = (nano * Decimal.fromInt 1000) :: Decimal
milli = (micro * Decimal.fromInt 1000) :: Decimal
second = (milli * Decimal.fromInt 1000) :: Decimal
minute = (second * Decimal.fromInt 60) :: Decimal
hour = (minute * Decimal.fromInt 60) :: Decimal
day = (hour * Decimal.fromInt 24) :: Decimal
week = (day * Decimal.fromInt 7) :: Decimal

bigIntToDecimal :: BigInt -> Decimal
bigIntToDecimal = fromMaybe zero -- the conversion should never fail
                  <<< Decimal.fromString <<< BigInt.toString

decimalToBigInt :: Decimal -> BigInt
decimalToBigInt = fromMaybe zero -- the conversion should never fail
                  <<< BigInt.fromString <<< Decimal.toString

toNanosecondsD :: PreciseDuration -> Decimal
toNanosecondsD =
  case _ of
    Nanoseconds d -> bigIntToDecimal d
    Microseconds d -> d * micro
    Milliseconds d -> d * milli
    Seconds d -> d * second
    Minutes d -> d * minute
    Hours d -> d * hour
    Days d -> d * day
    Weeks d -> d * week

toNanosecondsBI :: PreciseDuration -> BigInt
toNanosecondsBI = case _ of
  Nanoseconds d -> d
  other         -> decimalToBigInt $ toNanosecondsD other

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
