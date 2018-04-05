module Data.Time.PreciseDuration.Internal where

import Prelude

import Data.BigInt (BigInt)
import Data.BigInt as BigInt
import Data.Decimal (Decimal)
import Data.Decimal as Decimal
import Data.Maybe (fromMaybe)

data PreciseDuration
  = Nanoseconds Decimal -- BigInt -- this is BigInt to prevent fractional nanoseconds
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

-- Each duration in nanoseconds
nano   = Decimal.fromInt 1 :: Decimal
micro  = (nano * Decimal.fromInt 1000) :: Decimal
milli  = (micro * Decimal.fromInt 1000) :: Decimal
second = (milli * Decimal.fromInt 1000) :: Decimal
minute = (second * Decimal.fromInt 60) :: Decimal
hour   = (minute * Decimal.fromInt 60) :: Decimal
day    = (hour * Decimal.fromInt 24) :: Decimal
week   = (day * Decimal.fromInt 7) :: Decimal

-- bigIntToDecimal :: BigInt -> Decimal
-- bigIntToDecimal = fromMaybe zero -- the conversion should never fail
--                   <<< Decimal.fromString <<< BigInt.toString

-- decimalToBigInt :: Decimal -> BigInt
-- decimalToBigInt = fromMaybe zero  -- this case should never be hit
--                   <<< BigInt.fromString <<< Decimal.toString
--                   <<< Decimal.truncated -- the conversion will lose the mantissa

-- Note: While toNanosecondsD can be defined in terms of toNanosecondsBI (and vice
-- versa), we do not do so in order to avoid converting between those types.

toNanosecondsD :: PreciseDuration -> Decimal
toNanosecondsD = case _ of
  Nanoseconds d  -> d
  Microseconds d -> d * micro
  Milliseconds d -> d * milli
  Seconds d      -> d * second
  Minutes d      -> d * minute
  Hours d        -> d * hour
  Days d         -> d * day
  Weeks d        -> d * week

-- toNanosecondsBI :: PreciseDuration -> BigInt
-- toNanosecondsBI = case _ of
--   Nanoseconds d  -> d
--   -- these are ok because we first convert to nanoseconds, which cannot be fractional
--   Microseconds d -> decimalToBigInt $ d * micro
--   Milliseconds d -> decimalToBigInt $ d * milli
--   Seconds d      -> decimalToBigInt $ d * second
--   Minutes d      -> decimalToBigInt $ d * minute
--   Hours d        -> decimalToBigInt $ d * hour
--   Days d         -> decimalToBigInt $ d * day
--   Weeks d        -> decimalToBigInt $ d * week
