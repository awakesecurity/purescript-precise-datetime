module Data.Time.PreciseDuration where

import Prelude

import Data.BigInt (BigInt, fromInt)

data PreciseDuration
  = Nanoseconds BigInt
  | Microseconds BigInt
  | Milliseconds BigInt
  | Seconds BigInt
  | Minutes BigInt
  | Hours BigInt
  | Days BigInt
  | Weeks BigInt

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

unPreciseDuration :: PreciseDuration -> BigInt
unPreciseDuration = case _ of
  Nanoseconds d -> d
  Microseconds d -> d
  Milliseconds d -> d
  Seconds d -> d
  Minutes d -> d
  Hours d -> d
  Days d -> d
  Weeks d -> d

-- Each duration in nanoseconds
nano = fromInt 1 :: BigInt
micro = (nano * fromInt 1000) :: BigInt
milli = (micro * fromInt 1000) :: BigInt
second = (milli * fromInt 1000) :: BigInt
minute = (second * fromInt 60) :: BigInt
hour = (minute * fromInt 60) :: BigInt
day = (hour * fromInt 24) :: BigInt
week = (day * fromInt 7) :: BigInt

toNanoseconds' :: PreciseDuration -> BigInt
toNanoseconds' duration = case duration of
  Nanoseconds d -> d
  Microseconds d -> d * micro
  Milliseconds d -> d * milli
  Seconds d -> d * second
  Minutes d -> d * minute
  Hours d -> d * hour
  Days d -> d * day
  Weeks d -> d * week

toNanoseconds :: PreciseDuration -> PreciseDuration
toNanoseconds = Nanoseconds <<< toNanoseconds'

toMicroseconds :: PreciseDuration -> PreciseDuration
toMicroseconds duration = Microseconds $ (toNanoseconds' duration) / micro

toMilliseconds :: PreciseDuration -> PreciseDuration
toMilliseconds duration = Milliseconds $ (toNanoseconds' duration) / milli

toSeconds :: PreciseDuration -> PreciseDuration
toSeconds duration = Seconds $ (toNanoseconds' duration) / second

toMinutes :: PreciseDuration -> PreciseDuration
toMinutes duration = Minutes $ (toNanoseconds' duration) / minute

toHours :: PreciseDuration -> PreciseDuration
toHours duration = Hours $ (toNanoseconds' duration) / hour

toDays :: PreciseDuration -> PreciseDuration
toDays duration = Days $ (toNanoseconds' duration) / day

toWeeks :: PreciseDuration -> PreciseDuration
toWeeks duration = Weeks $ (toNanoseconds' duration) / week
