module Data.PreciseDateTime
  ( PreciseDateTime(..)
  , adjust
  , fromRFC3339String
  , toRFC3339String
  , toDateTimeLossy
  , fromDateTime
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Array ((!!))
import Data.BigInt (BigInt, pow)
import Data.BigInt as BigInt
import Data.Char.Unicode (isDigit)
import Data.DateTime (DateTime)
import Data.DateTime as DateTime
import Data.Enum (toEnum)
import Data.Formatter.DateTime (format)
import Data.Int (decimal)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.PreciseDate.Component (Nanosecond)
import Data.PreciseDateTime.Internal (dateTimeFormatISO)
import Data.RFC3339String (RFC3339String(..), trim)
import Data.RFC3339String as RFC3339String
import Data.String (Pattern(Pattern), drop, length, split, takeWhile)
import Data.Time.Duration as Duration
import Data.Time.PreciseDuration (PreciseDuration, toMilliseconds, toNanoseconds, unPreciseDuration)

data PreciseDateTime = PreciseDateTime DateTime Nanosecond

derive instance eqPreciseDateTime :: Eq PreciseDateTime
derive instance ordPreciseDateTime :: Ord PreciseDateTime

instance boundedPreciseDateTime :: Bounded PreciseDateTime where
  bottom = PreciseDateTime bottom bottom
  top = PreciseDateTime top top

instance showPreciseDateTime :: Show PreciseDateTime where
  show (PreciseDateTime dateTime ns) = "PreciseDateTime (" <> show dateTime <> ") " <> show ns

nanoStringPadding = "000000000" :: String

padNanoString :: (String -> String -> String) -> String -> String
padNanoString fn string =
  let padding = drop (length string) nanoStringPadding
  in fn string padding

leftPadNanoString :: String -> String
leftPadNanoString = padNanoString (flip append)

rightPadNanoString :: String -> String
rightPadNanoString = padNanoString append

parseSubseconds :: RFC3339String -> Maybe Int
parseSubseconds (RFC3339String s) = do
  let parts = split (Pattern ".") s
  afterDot <- parts !! 1
  let digits = takeWhile isDigit afterDot
  Int.fromString <<< rightPadNanoString $ digits

-- | Convert to `Nanosecond` separately so that a failure to parse subseconds
-- | results in `Just (Nanosecond 0)` instead of `Nothing`. This accounts for
-- | when subseconds were omitted from the timestamp, and allows us to
-- | differentiate between invalid `Int`s and invalid `Nanosecond`s.
nanosecond :: RFC3339String -> Maybe Nanosecond
nanosecond rfcString = parseSubseconds rfcString <|> Just 0 >>= toEnum

fromRFC3339String :: RFC3339String -> Maybe PreciseDateTime
fromRFC3339String rfcString = do
  dateTime <- RFC3339String.toDateTime rfcString
  ns  <- nanosecond rfcString
  pure $ PreciseDateTime dateTime ns

toRFC3339String :: PreciseDateTime -> RFC3339String
toRFC3339String (PreciseDateTime dateTime ns) =
  let
    beforeDot = format dateTimeFormatISO dateTime
    nanos = Int.toStringAs decimal (unwrap ns)
    leftPadded = leftPadNanoString nanos
  in
     trim <<< RFC3339String $ beforeDot <> "." <> leftPadded <> "Z"

-- | Adjusts a date/time value with a duration offset. `Nothing` is returned
-- | if the resulting date would be outside of the range of valid dates.
adjust :: PreciseDuration -> PreciseDateTime -> Maybe PreciseDateTime
adjust pd (PreciseDateTime dt ns) = do
  let
    nsPrecDur = toNanoseconds pd
    nsPrecDurInt = unPreciseDuration nsPrecDur
    msPrecDur = toMilliseconds nsPrecDur
    msPrecDurInt = unPreciseDuration msPrecDur
    roundTripDurInt = unPreciseDuration <<< toNanoseconds $ msPrecDur

    negative = nsPrecDurInt < zero
    msModTen = msPrecDurInt `mod` ten
    nsDiff = nsPrecDurInt - roundTripDurInt

    -- If the duration is negative, the duration in milliseconds is a multiple
    -- of 10, and the conversion from nanoseconds to milliseconds and back is
    -- not lossless, then we need to round up the lost nanoseconds to 1
    -- millisecond and adjust the duration.
    adjustment =
      if negative && msModTen == zero && nsDiff < zero
         then 1
         else 0

    adjustedMsPrecDurInt = msPrecDurInt - BigInt.fromInt adjustment

    msDur :: Duration.Milliseconds
    msDur = Duration.Milliseconds <<< BigInt.toNumber $ adjustedMsPrecDurInt

  adjustedDateTime <- DateTime.adjust msDur dt

  -- If the duration is larger than can be represented in a `Nanosecond`
  -- component, take the last 9 digits.
  let
    unsigned = BigInt.abs nsPrecDurInt
    nsString = BigInt.toString unsigned

    lastNine =
      if unsigned > maxNano
        then drop (length nsString - 9) nsString
        else nsString

  adjustedNsPrecDurInt <- BigInt.fromString lastNine
  let adjustedNsInt = BigInt.fromInt (unwrap ns) + adjustedNsPrecDurInt

  let
    inverted =
      if negative && adjustedNsInt <= maxNano && adjustedNsPrecDurInt /= zero
         then tenPowNine - adjustedNsInt
         else adjustedNsInt

  adjustedNs <- toInt inverted >>= toEnum
  pure (PreciseDateTime adjustedDateTime adjustedNs)

  where
    zero = BigInt.fromInt 0
    ten = BigInt.fromInt 10
    tenPowNine = ten `pow` BigInt.fromInt 9
    maxNano = tenPowNine - BigInt.fromInt 1
    maxm = ten `pow` BigInt.fromInt 22

    -- | `Data.BigInt` only provides `toNumber`, which loses precision for
    -- | numbers which are too large meaning that
    -- | `Int.fromNumber <<< BigInt.toNumber` could produce a valid `Int` but
    -- | would lose precision.
    toInt :: BigInt -> Maybe Int
    toInt = Int.fromString <<< BigInt.toString

toDateTimeLossy :: PreciseDateTime -> DateTime
toDateTimeLossy (PreciseDateTime dt _) = dt

fromDateTime :: DateTime -> PreciseDateTime
fromDateTime dt = PreciseDateTime dt bottom
