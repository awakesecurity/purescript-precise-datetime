module Test.Data.PreciseDateTime.Spec where

import Prelude

import Data.BigInt (fromInt, fromString)
import Data.Date as Date
import Data.DateTime (Date, Day, Month, Year, exactDate)
import Data.Enum (class BoundedEnum, toEnum)
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (class Newtype)
import Data.PreciseDateTime (PreciseDateTime, adjust, fromDateTime, fromRFC3339String, mkPreciseDateTime, toDateTimeLossy, toRFC3339String)
import Data.RFC3339String (RFC3339String(..))
import Data.Time.PreciseDuration (PreciseDuration(..))
import Partial.Unsafe (unsafePartial)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Utils (mkDateTime)

unsafeExactDate :: Year -> Month -> Day -> Date
unsafeExactDate y m d = unsafePartial fromJust $ exactDate y m d

unsafeToEnum :: forall a. BoundedEnum a => Int -> a
unsafeToEnum = unsafePartial fromJust <<< toEnum

unsafeMkPreciseDateTime :: Int -> Month -> Int -> Int -> Int -> Int -> Int -> PreciseDateTime
unsafeMkPreciseDateTime yyyy month dd hh mm ss ns = mkPreciseDateTime d h m s n
  where
    d = unsafeExactDate (unsafeToEnum yyyy) month (unsafeToEnum dd)
    h = unsafeToEnum hh
    m = unsafeToEnum mm
    s = unsafeToEnum ss
    n = unsafeToEnum ns

preciseDateTimeFixture :: Int -> PreciseDateTime
preciseDateTimeFixture = unsafeMkPreciseDateTime 1985 Date.March 13 12 34 56

dateStringFixture = "1985-03-13T12:34:56" :: String

newtype SecondsAndNanos = SecondsAndNanos { seconds :: String, nanos :: Int }

derive instance newtypeSecondsAndNanos :: Newtype SecondsAndNanos _
derive instance eqSecondsAndNanos :: Eq SecondsAndNanos
instance showSecondsAndNanos :: Show SecondsAndNanos where
  show (SecondsAndNanos { seconds, nanos }) = "{ seconds: " <> show seconds <> ", nanos: " <> show nanos <> " }"

spec :: forall r. Spec r Unit
spec =
  describe "PreciseDateTime" do
    describe "mkPreciseDateTime" do
      describe "works as expected with toDateTimeLossy" do
        it "when rounding" do
          let
            year = unsafeToEnum 2018
            month = Date.March
            day = unsafeToEnum 2
            hour = unsafeToEnum 9
            minute = unsafeToEnum 56
            second = unsafeToEnum 37
            nanosecond = unsafeToEnum 876543211

            date = unsafeExactDate year month day
            dateTime = mkDateTime 2018 Date.March 2 9 56 37 877

          toDateTimeLossy (mkPreciseDateTime date hour minute second nanosecond)
            `shouldEqual` dateTime

        it "when not rounding" do
          let
            year = unsafeToEnum 2018
            month = Date.March
            day = unsafeToEnum 2
            hour = unsafeToEnum 9
            minute = unsafeToEnum 56
            second = unsafeToEnum 37
            nanosecond = unsafeToEnum 123456789

            date = unsafeExactDate year month day
            dateTime = mkDateTime 2018 Date.March 2 9 56 37 123

          toDateTimeLossy (mkPreciseDateTime date hour minute second nanosecond)
            `shouldEqual` dateTime

    it "fromDateTime" do
      let
        yyyy = 2018
        month = Date.March
        dd = 2
        hh = 9
        mm = 56
        ss = 37
        ms = 123
        ns = 123000000

        dateTime = mkDateTime yyyy month dd hh mm ss ms
        preciseDateTime = unsafeMkPreciseDateTime yyyy month dd hh mm ss ns

      fromDateTime dateTime `shouldEqual` preciseDateTime

    it "fromRFC3339String" do
      fromRFC3339String (RFC3339String $ dateStringFixture <> "Z")
        `shouldEqual` (Just $ preciseDateTimeFixture 0)

      fromRFC3339String (RFC3339String $ dateStringFixture <> ".")
        `shouldEqual` Nothing

      fromRFC3339String (RFC3339String $ dateStringFixture <> ".0Z")
        `shouldEqual` (Just $ preciseDateTimeFixture 0)

      fromRFC3339String (RFC3339String $ dateStringFixture <> ".1Z")
        `shouldEqual` (Just $ preciseDateTimeFixture 100000000)

      fromRFC3339String (RFC3339String $ dateStringFixture <> ".01Z")
        `shouldEqual` (Just $ preciseDateTimeFixture 10000000)

      fromRFC3339String (RFC3339String $ dateStringFixture <> ".001Z")
        `shouldEqual` (Just $ preciseDateTimeFixture 1000000)

      fromRFC3339String (RFC3339String $ dateStringFixture <> ".10Z")
        `shouldEqual` (Just $ preciseDateTimeFixture 100000000)

      fromRFC3339String (RFC3339String $ dateStringFixture <> ".100Z")
        `shouldEqual` (Just $ preciseDateTimeFixture 100000000)

      fromRFC3339String (RFC3339String $ dateStringFixture <> ".123Z")
        `shouldEqual` (Just $ preciseDateTimeFixture 123000000)

      fromRFC3339String (RFC3339String $ dateStringFixture <> ".999999999Z")
        `shouldEqual` (Just $ preciseDateTimeFixture 999999999)

      fromRFC3339String (RFC3339String $ dateStringFixture <> ".000000001Z")
        `shouldEqual` (Just $ preciseDateTimeFixture 1)

      fromRFC3339String (RFC3339String $ dateStringFixture <> ".1000000000Z")
        `shouldEqual` Nothing

    it "toRFC3339String" do
      toRFC3339String (preciseDateTimeFixture 0)
        `shouldEqual` (RFC3339String $ dateStringFixture <> ".0Z")

      toRFC3339String (preciseDateTimeFixture 123000000)
        `shouldEqual` (RFC3339String $ dateStringFixture <> ".123Z")

      toRFC3339String (preciseDateTimeFixture 999999999)
        `shouldEqual` (RFC3339String $ dateStringFixture <> ".999999999Z")

      toRFC3339String (preciseDateTimeFixture 1)
        `shouldEqual` (RFC3339String $ dateStringFixture <> ".000000001Z")

      toRFC3339String (preciseDateTimeFixture 456000)
        `shouldEqual` (RFC3339String $ dateStringFixture <> ".000456Z")

      toRFC3339String (preciseDateTimeFixture 456009)
        `shouldEqual` (RFC3339String $ dateStringFixture <> ".000456009Z")

    it "adjust" do
      adjust (Nanoseconds <<< fromInt $ 0) (unsafeMkPreciseDateTime 1985 Date.March 13 0 0 0 0)
        `shouldEqual` (Just $ unsafeMkPreciseDateTime 1985 Date.March 13 0 0 0 0)

      adjust (Nanoseconds <<< fromInt $ 1) (unsafeMkPreciseDateTime 1985 Date.March 13 0 0 0 0)
        `shouldEqual` (Just $ unsafeMkPreciseDateTime 1985 Date.March 13 0 0 0 1)

      adjust (Nanoseconds <<< fromInt $ -1) (unsafeMkPreciseDateTime 1985 Date.March 13 0 0 0 0)
        `shouldEqual` (Just $ unsafeMkPreciseDateTime 1985 Date.March 12 23 59 59 999999999)

      adjust (Nanoseconds <<< fromInt $ 1000000) (unsafeMkPreciseDateTime 1985 Date.March 13 0 0 0 0)
        `shouldEqual` (Just $ unsafeMkPreciseDateTime 1985 Date.March 13 0 0 0 1000000)

      adjust(Nanoseconds <<< fromInt $ -1000000) (unsafeMkPreciseDateTime 1985 Date.March 13 0 0 0 0)
        `shouldEqual` (Just $ unsafeMkPreciseDateTime 1985 Date.March 12 23 59 59 999000000)

      adjust (Nanoseconds <<< fromInt $ 10000000) (unsafeMkPreciseDateTime 1985 Date.March 13 0 0 0 0)
        `shouldEqual` (Just $ unsafeMkPreciseDateTime 1985 Date.March 13 0 0 0 10000000)

      adjust (Nanoseconds <<< fromInt $ -10000000) (unsafeMkPreciseDateTime 1985 Date.March 13 0 0 0 0)
        `shouldEqual` (Just $ unsafeMkPreciseDateTime 1985 Date.March 12 23 59 59 990000000)

      adjust (Nanoseconds <<< fromInt $ 100000000) (unsafeMkPreciseDateTime 1985 Date.March 13 0 0 0 0)
        `shouldEqual` (Just $ unsafeMkPreciseDateTime 1985 Date.March 13 0 0 0 100000000)

      adjust (Nanoseconds <<< fromInt $ -100000000) (unsafeMkPreciseDateTime 1985 Date.March 13 0 0 0 0)
        `shouldEqual` (Just $ unsafeMkPreciseDateTime 1985 Date.March 12 23 59 59 900000000)

      adjust (Nanoseconds <<< fromInt $ 123456789) (unsafeMkPreciseDateTime 1985 Date.March 13 0 0 0 0)
        `shouldEqual` (Just $ unsafeMkPreciseDateTime 1985 Date.March 13 0 0 0 123456789)

      adjust (Nanoseconds <<< fromInt $ -123456789) (unsafeMkPreciseDateTime 1985 Date.March 13 0 0 0 0)
        `shouldEqual` (Just $ unsafeMkPreciseDateTime 1985 Date.March 12 23 59 59 876543211)

      adjust (Nanoseconds <<< fromInt $ 999999999) (unsafeMkPreciseDateTime 1985 Date.March 13 0 0 0 0)
        `shouldEqual` (Just $ unsafeMkPreciseDateTime 1985 Date.March 13 0 0 0 999999999)

      adjust (Nanoseconds <<< fromInt $ -999999999) (unsafeMkPreciseDateTime 1985 Date.March 13 0 0 0 0)
        `shouldEqual` (Just $ unsafeMkPreciseDateTime 1985 Date.March 12 23 59 59 1)

      adjust (Nanoseconds <<< fromInt $ 1000000000) (unsafeMkPreciseDateTime 1985 Date.March 13 0 0 0 0)
        `shouldEqual` (Just $ unsafeMkPreciseDateTime 1985 Date.March 13 0 0 1 0)

      adjust (Nanoseconds <<< fromInt $ -1000000000) (unsafeMkPreciseDateTime 1985 Date.March 13 0 0 0 0)
        `shouldEqual` (Just $ unsafeMkPreciseDateTime 1985 Date.March 12 23 59 59 0)

      adjust (Nanoseconds <<< fromInt $ 1000000001) (unsafeMkPreciseDateTime 1985 Date.March 13 0 0 0 0)
        `shouldEqual` (Just $ unsafeMkPreciseDateTime 1985 Date.March 13 0 0 1 1)

      adjust (Nanoseconds <<< fromInt $ -1000000001) (unsafeMkPreciseDateTime 1985 Date.March 13 0 0 0 0)
        `shouldEqual` (Just $ unsafeMkPreciseDateTime 1985 Date.March 12 23 59 58 999999999)

      adjust (Nanoseconds <<< fromInt $ -1000000002) (unsafeMkPreciseDateTime 1985 Date.March 13 0 0 0 0)
        `shouldEqual` (Just $ unsafeMkPreciseDateTime 1985 Date.March 12 23 59 58 999999998)

      adjust (Nanoseconds <<< fromInt $ 1000000010) (unsafeMkPreciseDateTime 1985 Date.March 13 0 0 0 0)
        `shouldEqual` (Just $ unsafeMkPreciseDateTime 1985 Date.March 13 0 0 1 10)

      adjust (Nanoseconds <<< fromInt $ -1000000010) (unsafeMkPreciseDateTime 1985 Date.March 13 0 0 0 0)
        `shouldEqual` (Just $ unsafeMkPreciseDateTime 1985 Date.March 12 23 59 58 999999990)

      adjust (Nanoseconds <<< fromInt $ 1000000100) (unsafeMkPreciseDateTime 1985 Date.March 13 0 0 0 0)
        `shouldEqual` (Just $ unsafeMkPreciseDateTime 1985 Date.March 13 0 0 1 100)

      adjust (Nanoseconds <<< fromInt $ -1000000100) (unsafeMkPreciseDateTime 1985 Date.March 13 0 0 0 0)
        `shouldEqual` (Just $ unsafeMkPreciseDateTime 1985 Date.March 12 23 59 58 999999900)

      adjust (Nanoseconds <<< fromInt $ -1000001000) (unsafeMkPreciseDateTime 1985 Date.March 13 0 0 0 0)
        `shouldEqual` (Just $ unsafeMkPreciseDateTime 1985 Date.March 12 23 59 58 999999000)

      adjust (Nanoseconds <<< fromInt $ -1000010000) (unsafeMkPreciseDateTime 1985 Date.March 13 0 0 0 0)
        `shouldEqual` (Just $ unsafeMkPreciseDateTime 1985 Date.March 12 23 59 58 999990000)

      adjust (Nanoseconds <<< fromInt $ -1000100000) (unsafeMkPreciseDateTime 1985 Date.March 13 0 0 0 0)
        `shouldEqual` (Just $ unsafeMkPreciseDateTime 1985 Date.March 12 23 59 58 999900000)

      adjust (Nanoseconds <<< fromInt $ -1001000000) (unsafeMkPreciseDateTime 1985 Date.March 13 0 0 0 0)
        `shouldEqual` (Just $ unsafeMkPreciseDateTime 1985 Date.March 12 23 59 58 999000000)

      adjust (Nanoseconds <<< fromInt $ -1010000000) (unsafeMkPreciseDateTime 1985 Date.March 13 0 0 0 0)
        `shouldEqual` (Just $ unsafeMkPreciseDateTime 1985 Date.March 12 23 59 58 990000000)

      adjust (Nanoseconds <<< unsafePartial fromJust <<< fromString $ "-10000000000") (unsafeMkPreciseDateTime 1985 Date.March 13 0 0 0 0)
        `shouldEqual` (Just $ unsafeMkPreciseDateTime 1985 Date.March 12 23 59 50 0)

      adjust (Nanoseconds <<< unsafePartial fromJust <<< fromString $ "60000000000") (unsafeMkPreciseDateTime 1985 Date.March 13 0 0 0 0)
        `shouldEqual` (Just $ unsafeMkPreciseDateTime 1985 Date.March 13 0 1 0 0)

      adjust (Nanoseconds <<< unsafePartial fromJust <<< fromString $ "-60000000000") (unsafeMkPreciseDateTime 1985 Date.March 13 0 0 0 0)
        `shouldEqual` (Just $ unsafeMkPreciseDateTime 1985 Date.March 12 23 59 0 0)

      adjust (Nanoseconds <<< unsafePartial fromJust <<< fromString $ "60000000001") (unsafeMkPreciseDateTime 1985 Date.March 13 0 0 0 0)
        `shouldEqual` (Just $ unsafeMkPreciseDateTime 1985 Date.March 13 0 1 0 1)

      adjust (Nanoseconds <<< unsafePartial fromJust <<< fromString $ "-60000000001") (unsafeMkPreciseDateTime 1985 Date.March 13 0 0 0 0)
        `shouldEqual` (Just $ unsafeMkPreciseDateTime 1985 Date.March 12 23 58 59 999999999)

      adjust (Nanoseconds <<< unsafePartial fromJust <<< fromString $ "3600000000000") (unsafeMkPreciseDateTime 1985 Date.March 13 0 0 0 0)
        `shouldEqual` (Just $ unsafeMkPreciseDateTime 1985 Date.March 13 1 0 0 0)

      adjust (Nanoseconds <<< unsafePartial fromJust <<< fromString $ "-3600000000000") (unsafeMkPreciseDateTime 1985 Date.March 13 0 0 0 0)
        `shouldEqual` (Just $ unsafeMkPreciseDateTime 1985 Date.March 12 23 0 0 0)

      adjust (Nanoseconds <<< unsafePartial fromJust <<< fromString $ "3600000000001") (unsafeMkPreciseDateTime 1985 Date.March 13 0 0 0 0)
        `shouldEqual` (Just $ unsafeMkPreciseDateTime 1985 Date.March 13 1 0 0 1)

      adjust (Nanoseconds <<< unsafePartial fromJust <<< fromString $ "-3600000000001") (unsafeMkPreciseDateTime 1985 Date.March 13 0 0 0 0)
        `shouldEqual` (Just $ unsafeMkPreciseDateTime 1985 Date.March 12 22 59 59 999999999)

      adjust (Nanoseconds <<< unsafePartial fromJust <<< fromString $ "86400000000000") (unsafeMkPreciseDateTime 1985 Date.March 13 0 0 0 0)
        `shouldEqual` (Just $ unsafeMkPreciseDateTime 1985 Date.March 14 0 0 0 0)

      adjust (Nanoseconds <<< unsafePartial fromJust <<< fromString $ "-86400000000000") (unsafeMkPreciseDateTime 1985 Date.March 13 0 0 0 0)
        `shouldEqual` (Just $ unsafeMkPreciseDateTime 1985 Date.March 12 0 0 0 0)

      adjust (Nanoseconds <<< unsafePartial fromJust <<< fromString $ "86400000000001") (unsafeMkPreciseDateTime 1985 Date.March 13 0 0 0 0)
        `shouldEqual` (Just $ unsafeMkPreciseDateTime 1985 Date.March 14 0 0 0 1)

      adjust (Nanoseconds <<< unsafePartial fromJust <<< fromString $ "-86400000000001") (unsafeMkPreciseDateTime 1985 Date.March 13 0 0 0 0)
        `shouldEqual` (Just $ unsafeMkPreciseDateTime 1985 Date.March 11 23 59 59 999999999)

      adjust (Nanoseconds <<< unsafePartial fromJust <<< fromString $ "604800000000000") (unsafeMkPreciseDateTime 1985 Date.March 13 0 0 0 0)
        `shouldEqual` (Just $ unsafeMkPreciseDateTime 1985 Date.March 20 0 0 0 0)

      adjust (Nanoseconds <<< unsafePartial fromJust <<< fromString $ "-604800000000000") (unsafeMkPreciseDateTime 1985 Date.March 13 0 0 0 0)
        `shouldEqual` (Just $ unsafeMkPreciseDateTime 1985 Date.March 6 0 0 0 0)

      adjust (Nanoseconds <<< unsafePartial fromJust <<< fromString $ "604800000000001") (unsafeMkPreciseDateTime 1985 Date.March 13 0 0 0 0)
        `shouldEqual` (Just $ unsafeMkPreciseDateTime 1985 Date.March 20 0 0 0 1)

      adjust (Nanoseconds <<< unsafePartial fromJust <<< fromString $ "-604800000000001") (unsafeMkPreciseDateTime 1985 Date.March 13 0 0 0 0)
        `shouldEqual` (Just $ unsafeMkPreciseDateTime 1985 Date.March 5 23 59 59 999999999)

      adjust (Nanoseconds <<< unsafePartial fromJust <<< fromString $ "-300000000000") (unsafeMkPreciseDateTime 2017 Date.September 17 0 0 0 0)
        `shouldEqual` (Just $ unsafeMkPreciseDateTime 2017 Date.September 16 23 55 0 0)

      adjust (Nanoseconds <<< unsafePartial fromJust <<< fromString $ "-300000000000") (unsafeMkPreciseDateTime 2017 Date.September 17 0 0 0 123000000)
        `shouldEqual` (Just $ unsafeMkPreciseDateTime 2017 Date.September 16 23 55 0 123000000)

      adjust (Nanoseconds <<< unsafePartial fromJust <<< fromString $ "-300000000000") (unsafeMkPreciseDateTime 2017 Date.September 17 0 0 0 123456789)
        `shouldEqual` (Just $ unsafeMkPreciseDateTime 2017 Date.September 16 23 55 0 123456789)
