module Data.Time.PreciseDuration.Internal where

import Prelude

import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Decimal (Decimal)
import Data.Decimal as Decimal
import Data.Maybe (fromMaybe)

data PreciseDuration
  = Nanoseconds Decimal
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

-- Each duration in nanoseconds
nano   = Decimal.fromInt 1 :: Decimal
micro  = (nano * Decimal.fromInt 1000) :: Decimal
milli  = (micro * Decimal.fromInt 1000) :: Decimal
second = (milli * Decimal.fromInt 1000) :: Decimal
minute = (second * Decimal.fromInt 60) :: Decimal
hour   = (minute * Decimal.fromInt 60) :: Decimal
day    = (hour * Decimal.fromInt 24) :: Decimal
week   = (day * Decimal.fromInt 7) :: Decimal
