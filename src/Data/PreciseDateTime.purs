module Data.PreciseDateTime
  ( PreciseDateTime(..)
  , adjust
  , diff
  , fromRFC3339String
  , toRFC3339String
  , toDateTimeLossy
  , fromDateTime
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Array ((!!))
import Data.CodePoint.Unicode (isDecDigit)
import Data.DateTime (DateTime, millisecond, time)
import Data.DateTime as DateTime
import Data.Decimal as Decimal
import Data.Enum (fromEnum, toEnum)
import Data.Formatter.DateTime (format)
import Data.Int (decimal)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.PreciseDate.Component (Nanosecond(..))
import Data.PreciseDateTime.Internal (dateTimeFormatISO)
import Data.RFC3339String (RFC3339String(..), trim)
import Data.RFC3339String as RFC3339String
import Data.String (Pattern(..), codePointFromChar, split)
import Data.String.CodeUnits (drop, length, take, takeWhile)
import Data.Time.Duration (Milliseconds(..))
import Data.Time.Duration as Duration
import Data.Time.PreciseDuration (PreciseDuration)
import Data.Time.PreciseDuration as PD

data PreciseDateTime = PreciseDateTime DateTime Nanosecond

derive instance eqPreciseDateTime :: Eq PreciseDateTime
derive instance ordPreciseDateTime :: Ord PreciseDateTime

instance boundedPreciseDateTime :: Bounded PreciseDateTime where
  bottom = PreciseDateTime bottom bottom
  top = PreciseDateTime top top

instance showPreciseDateTime :: Show PreciseDateTime where
  show (PreciseDateTime dateTime ns) = "PreciseDateTime (" <> show dateTime <> ") " <> show ns

milliStringPadding = "000" :: String
nanoStringPadding = "000000" :: String
subsecondStringPadding = "000000000" :: String

padString :: String -> (String -> String -> String) -> String -> String
padString padding fn string = fn string $ drop (length string) padding

padMilliString :: (String -> String -> String) -> String -> String
padMilliString = padString milliStringPadding

padNanoString :: (String -> String -> String) -> String -> String
padNanoString = padString nanoStringPadding

padSubsecondString :: (String -> String -> String) -> String -> String
padSubsecondString = padString subsecondStringPadding

leftPadMilliString :: String -> String
leftPadMilliString = padMilliString (flip append)

leftPadNanoString :: String -> String
leftPadNanoString = padNanoString (flip append)

rightPadSubsecondString :: String -> String
rightPadSubsecondString = padSubsecondString append

parseSubseconds :: RFC3339String -> Maybe String
parseSubseconds (RFC3339String s) = do
  let parts = split (Pattern ".") s
  afterDot <- parts !! 1
  let digits = takeWhile (isDecDigit <<< codePointFromChar) afterDot
  pure $ rightPadSubsecondString $ take 9 digits

-- | Convert to `Nanosecond` separately so that a failure to parse subseconds
-- | results in `Just (Nanosecond 0)` instead of `Nothing`. This accounts for
-- | when subseconds were omitted from the timestamp, and allows us to
-- | differentiate between invalid `Int`s and invalid `Nanosecond`s.
nanosecond :: RFC3339String -> Maybe Nanosecond
nanosecond rfcString = nanoseconds rfcString <|> Just 0 >>= toEnum
  where
  nanoseconds = parseSubseconds >=> drop 3 >>> Int.fromString

fromRFC3339String :: RFC3339String -> Maybe PreciseDateTime
fromRFC3339String rfcString = do
  dateTime <- RFC3339String.toDateTime rfcString
  ns <- nanosecond rfcString
  pure $ PreciseDateTime dateTime ns

toRFC3339String :: PreciseDateTime -> RFC3339String
toRFC3339String (PreciseDateTime dateTime ns) =
  let
    beforeDot = format dateTimeFormatISO dateTime
    millis = Int.toStringAs decimal $ fromEnum $ millisecond $ time dateTime
    nanos = Int.toStringAs decimal $ unwrap ns
    leftPaddedMs = leftPadMilliString millis
    leftPaddedNs = leftPadNanoString nanos
  in
    trim <<< RFC3339String $ beforeDot <> "." <> leftPaddedMs <> leftPaddedNs <> "Z"

-- | Adjusts a date/time value with a duration offset. `Nothing` is returned
-- | if the resulting date would be outside of the range of valid dates.
adjust :: PreciseDuration -> PreciseDateTime -> Maybe PreciseDateTime
adjust pd (PreciseDateTime dt (Nanosecond ns)) = do
  let
    nanosDur = PD.toDecimalLossy (PD.toNanoseconds pd)
    millisPart = Decimal.truncated $ PD.toDecimalLossy $ PD.toMilliseconds pd
    nanosPart = Int.floor $ Decimal.toNumber $ nanosDur - (Decimal.fromInt 1_000_000 * millisPart)
    millisNum = Decimal.toNumber millisPart
    nanosAdj = ns + nanosPart
    { millis, nanos } =
      if nanosAdj >= 1_000_000 then
        { millis: millisNum + 1.0, nanos: nanosAdj - 1_000_000 }
      else if nanosAdj < 0 then do
        { millis: millisNum - 1.0, nanos: nanosAdj + 1_000_000 }
      else
        { millis: millisNum, nanos: nanosAdj }

  flip PreciseDateTime (Nanosecond nanos)
    <$> DateTime.adjust (Milliseconds millis) dt

diff :: PreciseDateTime -> PreciseDateTime -> PreciseDuration
diff (PreciseDateTime dt0 (Nanosecond ns0)) (PreciseDateTime dt1 (Nanosecond ns1)) =
  let
    Duration.Milliseconds msNum = DateTime.diff dt0 dt1
    nsNum = Int.toNumber (ns0 - ns1) + msNum * 1000000.0
  in
    PD.unsafeNanoseconds (Decimal.fromNumber nsNum)

toDateTimeLossy :: PreciseDateTime -> DateTime
toDateTimeLossy (PreciseDateTime dt _) = dt

fromDateTime :: DateTime -> PreciseDateTime
fromDateTime dt = PreciseDateTime dt bottom
