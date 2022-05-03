module Test.Data.PreciseDateTime.Spec where

import Prelude

import Data.Date as Date
import Data.Decimal (fromString)
import Data.Enum (toEnum)
import Data.Maybe (Maybe(..), fromJust)
import Data.Newtype (class Newtype)
import Data.PreciseDateTime (PreciseDateTime(..), adjust, diff, fromRFC3339String, toRFC3339String)
import Data.RFC3339String (RFC3339String(..))
import Data.Time.PreciseDuration as PD
import Partial.Unsafe (unsafePartial)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Utils (mkDateTime)

mkPreciseDateTime :: Int -> Date.Month -> Int -> Int -> Int -> Int -> Int -> Int -> PreciseDateTime
mkPreciseDateTime yyyy month dd hh mm ss ms ns =
  PreciseDateTime
    (mkDateTime yyyy month dd hh mm ss ms)
    (unsafePartial fromJust $ toEnum ns)

preciseDateTimeFixture :: Int -> Int -> PreciseDateTime
preciseDateTimeFixture = mkPreciseDateTime 1985 Date.March 13 12 34 56

dateStringFixture = "1985-03-13T12:34:56" :: String

newtype SecondsAndNanos = SecondsAndNanos { seconds :: String, nanos :: Int }

derive instance newtypeSecondsAndNanos :: Newtype SecondsAndNanos _
derive instance eqSecondsAndNanos :: Eq SecondsAndNanos
instance showSecondsAndNanos :: Show SecondsAndNanos where
  show (SecondsAndNanos { seconds, nanos }) = "{ seconds: " <> show seconds <> ", nanos: " <> show nanos <> " }"

spec :: Spec Unit
spec =
  describe "PreciseDateTime" do
    it "fromRFC3339String" do
      fromRFC3339String (RFC3339String $ dateStringFixture <> "Z")
        `shouldEqual` (Just $ preciseDateTimeFixture 0 0)

      fromRFC3339String (RFC3339String $ dateStringFixture <> ".")
        `shouldEqual` Nothing

      fromRFC3339String (RFC3339String $ dateStringFixture <> ".0Z")
        `shouldEqual` (Just $ preciseDateTimeFixture 0 0)

      fromRFC3339String (RFC3339String $ dateStringFixture <> ".1Z")
        `shouldEqual` (Just $ preciseDateTimeFixture 100 0)

      fromRFC3339String (RFC3339String $ dateStringFixture <> ".01Z")
        `shouldEqual` (Just $ preciseDateTimeFixture 10 0)

      fromRFC3339String (RFC3339String $ dateStringFixture <> ".001Z")
        `shouldEqual` (Just $ preciseDateTimeFixture 1 0)

      fromRFC3339String (RFC3339String $ dateStringFixture <> ".10Z")
        `shouldEqual` (Just $ preciseDateTimeFixture 100 0)

      fromRFC3339String (RFC3339String $ dateStringFixture <> ".100Z")
        `shouldEqual` (Just $ preciseDateTimeFixture 100 0)

      fromRFC3339String (RFC3339String $ dateStringFixture <> ".123Z")
        `shouldEqual` (Just $ preciseDateTimeFixture 123 0)

      fromRFC3339String (RFC3339String $ dateStringFixture <> ".999999999Z")
        `shouldEqual` (Just $ preciseDateTimeFixture 999 999999)

      fromRFC3339String (RFC3339String $ dateStringFixture <> ".000000001Z")
        `shouldEqual` (Just $ preciseDateTimeFixture 0 1)

      -- Take the first 9 digits if more than 9 are provided
      fromRFC3339String (RFC3339String $ dateStringFixture <> ".1000000000Z")
        `shouldEqual` (Just $ preciseDateTimeFixture 100 0)

    it "toRFC3339String" do
      toRFC3339String (preciseDateTimeFixture 0 0)
        `shouldEqual` (RFC3339String $ dateStringFixture <> ".0Z")

      toRFC3339String (preciseDateTimeFixture 123 0)
        `shouldEqual` (RFC3339String $ dateStringFixture <> ".123Z")

      toRFC3339String (preciseDateTimeFixture 999 999999)
        `shouldEqual` (RFC3339String $ dateStringFixture <> ".999999999Z")

      toRFC3339String (preciseDateTimeFixture 0 1)
        `shouldEqual` (RFC3339String $ dateStringFixture <> ".000000001Z")

      toRFC3339String (preciseDateTimeFixture 0 456000)
        `shouldEqual` (RFC3339String $ dateStringFixture <> ".000456Z")

      toRFC3339String (preciseDateTimeFixture 0 456009)
        `shouldEqual` (RFC3339String $ dateStringFixture <> ".000456009Z")

    it "diff" do
      diff (mkPreciseDateTime 1985 Date.March 13 0 0 0 0 0) (mkPreciseDateTime 1985 Date.March 13 0 0 0 0 0)
        `shouldEqual` (PD.nanoseconds 0)

      diff (mkPreciseDateTime 1985 Date.March 13 0 0 0 0 1) (mkPreciseDateTime 1985 Date.March 13 0 0 0 0 0)
        `shouldEqual` (PD.nanoseconds 1)

      diff (mkPreciseDateTime 1985 Date.March 13 0 0 0 0 0) (mkPreciseDateTime 1985 Date.March 13 0 0 0 0 1)
        `shouldEqual` (PD.nanoseconds (-1))

      diff (mkPreciseDateTime 1985 Date.March 12 23 59 59 999 999999) (mkPreciseDateTime 1985 Date.March 13 0 0 0 0 0)
        `shouldEqual` (PD.nanoseconds (-1))

      diff (mkPreciseDateTime 1985 Date.March 13 0 0 0 0 0) (mkPreciseDateTime 1985 Date.March 12 23 59 59 999 999999)
        `shouldEqual` (PD.nanoseconds 1)

      diff (mkPreciseDateTime 1985 Date.March 12 23 59 59 999 0) (mkPreciseDateTime 1985 Date.March 13 0 0 0 0 0)
        `shouldEqual` (PD.nanoseconds (-1000000))

      diff (mkPreciseDateTime 1985 Date.March 13 0 0 0 0 0) (mkPreciseDateTime 1985 Date.March 12 23 59 59 999 0)
        `shouldEqual` (PD.nanoseconds 1000000)

      diff (mkPreciseDateTime 1985 Date.March 13 0 0 1 0 1) (mkPreciseDateTime 1985 Date.March 13 0 0 0 0 0)
        `shouldEqual` (PD.nanoseconds 1000000001)

      diff (mkPreciseDateTime 1985 Date.March 13 0 0 0 0 0) (mkPreciseDateTime 1985 Date.March 13 0 0 1 0 1)
        `shouldEqual` (PD.nanoseconds (-1000000001))

      diff (mkPreciseDateTime 1985 Date.March 12 23 59 58 999 999999) (mkPreciseDateTime 1985 Date.March 13 0 0 0 0 0)
        `shouldEqual` (PD.nanoseconds (-1000000001))

      diff (mkPreciseDateTime 1985 Date.March 13 0 0 0 0 0) (mkPreciseDateTime 1985 Date.March 12 23 59 58 999 999999)
        `shouldEqual` (PD.nanoseconds 1000000001)

      diff (mkPreciseDateTime 1985 Date.March 20 0 0 0 0 1) (mkPreciseDateTime 1985 Date.March 13 0 0 0 0 0)
        `shouldEqual` (PD.unsafeNanoseconds <<< unsafePartial fromJust <<< fromString $ "604800000000001")

      diff (mkPreciseDateTime 1985 Date.March 5 23 59 59 999 999999) (mkPreciseDateTime 1985 Date.March 13 0 0 0 0 0)
        `shouldEqual` (PD.unsafeNanoseconds <<< unsafePartial fromJust <<< fromString $ "-604800000000001")

    it "adjust" do
      adjust (PD.nanoseconds 0) (mkPreciseDateTime 1985 Date.March 13 0 0 0 0 0)
        `shouldEqual` (Just $ mkPreciseDateTime 1985 Date.March 13 0 0 0 0 0)

      adjust (PD.nanoseconds 1) (mkPreciseDateTime 1985 Date.March 13 0 0 0 0 0)
        `shouldEqual` (Just $ mkPreciseDateTime 1985 Date.March 13 0 0 0 0 1)

      adjust (PD.nanoseconds (-1)) (mkPreciseDateTime 1985 Date.March 13 0 0 0 0 0)
        `shouldEqual` (Just $ mkPreciseDateTime 1985 Date.March 12 23 59 59 999 999999)

      adjust (PD.nanoseconds 1000000) (mkPreciseDateTime 1985 Date.March 13 0 0 0 0 0)
        `shouldEqual` (Just $ mkPreciseDateTime 1985 Date.March 13 0 0 0 1 0)

      adjust (PD.nanoseconds (-1000000)) (mkPreciseDateTime 1985 Date.March 13 0 0 0 0 0)
        `shouldEqual` (Just $ mkPreciseDateTime 1985 Date.March 12 23 59 59 999 0)

      adjust (PD.nanoseconds 10000000) (mkPreciseDateTime 1985 Date.March 13 0 0 0 0 0)
        `shouldEqual` (Just $ mkPreciseDateTime 1985 Date.March 13 0 0 0 10 0)

      adjust (PD.nanoseconds (-10000000)) (mkPreciseDateTime 1985 Date.March 13 0 0 0 0 0)
        `shouldEqual` (Just $ mkPreciseDateTime 1985 Date.March 12 23 59 59 990 0)

      adjust (PD.nanoseconds 100000000) (mkPreciseDateTime 1985 Date.March 13 0 0 0 0 0)
        `shouldEqual` (Just $ mkPreciseDateTime 1985 Date.March 13 0 0 0 100 0)

      adjust (PD.nanoseconds (-100000000)) (mkPreciseDateTime 1985 Date.March 13 0 0 0 0 0)
        `shouldEqual` (Just $ mkPreciseDateTime 1985 Date.March 12 23 59 59 900 0)

      adjust (PD.nanoseconds 123456789) (mkPreciseDateTime 1985 Date.March 13 0 0 0 0 0)
        `shouldEqual` (Just $ mkPreciseDateTime 1985 Date.March 13 0 0 0 123 456789)

      adjust (PD.nanoseconds (-123456789)) (mkPreciseDateTime 1985 Date.March 13 0 0 0 0 0)
        `shouldEqual` (Just $ mkPreciseDateTime 1985 Date.March 12 23 59 59 876 543211)

      adjust (PD.nanoseconds 999999999) (mkPreciseDateTime 1985 Date.March 13 0 0 0 0 0)
        `shouldEqual` (Just $ mkPreciseDateTime 1985 Date.March 13 0 0 0 999 999999)

      adjust (PD.nanoseconds (-999999999)) (mkPreciseDateTime 1985 Date.March 13 0 0 0 0 0)
        `shouldEqual` (Just $ mkPreciseDateTime 1985 Date.March 12 23 59 59 0 1)

      adjust (PD.nanoseconds 1000000000) (mkPreciseDateTime 1985 Date.March 13 0 0 0 0 0)
        `shouldEqual` (Just $ mkPreciseDateTime 1985 Date.March 13 0 0 1 0 0)

      adjust (PD.nanoseconds (-1000000000)) (mkPreciseDateTime 1985 Date.March 13 0 0 0 0 0)
        `shouldEqual` (Just $ mkPreciseDateTime 1985 Date.March 12 23 59 59 0 0)

      adjust (PD.nanoseconds 1000000001) (mkPreciseDateTime 1985 Date.March 13 0 0 0 0 0)
        `shouldEqual` (Just $ mkPreciseDateTime 1985 Date.March 13 0 0 1 0 1)

      adjust (PD.nanoseconds (-1000000001)) (mkPreciseDateTime 1985 Date.March 13 0 0 0 0 0)
        `shouldEqual` (Just $ mkPreciseDateTime 1985 Date.March 12 23 59 58 999 999999)

      adjust (PD.nanoseconds (-1000000002)) (mkPreciseDateTime 1985 Date.March 13 0 0 0 0 0)
        `shouldEqual` (Just $ mkPreciseDateTime 1985 Date.March 12 23 59 58 999 999998)

      adjust (PD.nanoseconds 1000000010) (mkPreciseDateTime 1985 Date.March 13 0 0 0 0 0)
        `shouldEqual` (Just $ mkPreciseDateTime 1985 Date.March 13 0 0 1 0 10)

      adjust (PD.nanoseconds (-1000000010)) (mkPreciseDateTime 1985 Date.March 13 0 0 0 0 0)
        `shouldEqual` (Just $ mkPreciseDateTime 1985 Date.March 12 23 59 58 999 999990)

      adjust (PD.nanoseconds 1000000100) (mkPreciseDateTime 1985 Date.March 13 0 0 0 0 0)
        `shouldEqual` (Just $ mkPreciseDateTime 1985 Date.March 13 0 0 1 0 100)

      adjust (PD.nanoseconds (-1000000100)) (mkPreciseDateTime 1985 Date.March 13 0 0 0 0 0)
        `shouldEqual` (Just $ mkPreciseDateTime 1985 Date.March 12 23 59 58 999 999900)

      adjust (PD.nanoseconds (-1000001000)) (mkPreciseDateTime 1985 Date.March 13 0 0 0 0 0)
        `shouldEqual` (Just $ mkPreciseDateTime 1985 Date.March 12 23 59 58 999 999000)

      adjust (PD.nanoseconds (-1000010000)) (mkPreciseDateTime 1985 Date.March 13 0 0 0 0 0)
        `shouldEqual` (Just $ mkPreciseDateTime 1985 Date.March 12 23 59 58 999 990000)

      adjust (PD.nanoseconds (-1000100000)) (mkPreciseDateTime 1985 Date.March 13 0 0 0 0 0)
        `shouldEqual` (Just $ mkPreciseDateTime 1985 Date.March 12 23 59 58 999 900000)

      adjust (PD.nanoseconds (-1001000000)) (mkPreciseDateTime 1985 Date.March 13 0 0 0 0 0)
        `shouldEqual` (Just $ mkPreciseDateTime 1985 Date.March 12 23 59 58 999 0)

      adjust (PD.nanoseconds (-1010000000)) (mkPreciseDateTime 1985 Date.March 13 0 0 0 0 0)
        `shouldEqual` (Just $ mkPreciseDateTime 1985 Date.March 12 23 59 58 990 0)

      adjust (PD.unsafeNanoseconds <<< unsafePartial fromJust <<< fromString $ "-10000000000") (mkPreciseDateTime 1985 Date.March 13 0 0 0 0 0)
        `shouldEqual` (Just $ mkPreciseDateTime 1985 Date.March 12 23 59 50 0 0)

      adjust (PD.unsafeNanoseconds <<< unsafePartial fromJust <<< fromString $ "60000000000") (mkPreciseDateTime 1985 Date.March 13 0 0 0 0 0)
        `shouldEqual` (Just $ mkPreciseDateTime 1985 Date.March 13 0 1 0 0 0)

      adjust (PD.unsafeNanoseconds <<< unsafePartial fromJust <<< fromString $ "-60000000000") (mkPreciseDateTime 1985 Date.March 13 0 0 0 0 0)
        `shouldEqual` (Just $ mkPreciseDateTime 1985 Date.March 12 23 59 0 0 0)

      adjust (PD.unsafeNanoseconds <<< unsafePartial fromJust <<< fromString $ "60000000001") (mkPreciseDateTime 1985 Date.March 13 0 0 0 0 0)
        `shouldEqual` (Just $ mkPreciseDateTime 1985 Date.March 13 0 1 0 0 1)

      adjust (PD.unsafeNanoseconds <<< unsafePartial fromJust <<< fromString $ "-60000000001") (mkPreciseDateTime 1985 Date.March 13 0 0 0 0 0)
        `shouldEqual` (Just $ mkPreciseDateTime 1985 Date.March 12 23 58 59 999 999999)

      adjust (PD.unsafeNanoseconds <<< unsafePartial fromJust <<< fromString $ "3600000000000") (mkPreciseDateTime 1985 Date.March 13 0 0 0 0 0)
        `shouldEqual` (Just $ mkPreciseDateTime 1985 Date.March 13 1 0 0 0 0)

      adjust (PD.unsafeNanoseconds <<< unsafePartial fromJust <<< fromString $ "-3600000000000") (mkPreciseDateTime 1985 Date.March 13 0 0 0 0 0)
        `shouldEqual` (Just $ mkPreciseDateTime 1985 Date.March 12 23 0 0 0 0)

      adjust (PD.unsafeNanoseconds <<< unsafePartial fromJust <<< fromString $ "3600000000001") (mkPreciseDateTime 1985 Date.March 13 0 0 0 0 0)
        `shouldEqual` (Just $ mkPreciseDateTime 1985 Date.March 13 1 0 0 0 1)

      adjust (PD.unsafeNanoseconds <<< unsafePartial fromJust <<< fromString $ "-3600000000001") (mkPreciseDateTime 1985 Date.March 13 0 0 0 0 0)
        `shouldEqual` (Just $ mkPreciseDateTime 1985 Date.March 12 22 59 59 999 999999)

      adjust (PD.unsafeNanoseconds <<< unsafePartial fromJust <<< fromString $ "86400000000000") (mkPreciseDateTime 1985 Date.March 13 0 0 0 0 0)
        `shouldEqual` (Just $ mkPreciseDateTime 1985 Date.March 14 0 0 0 0 0)

      adjust (PD.unsafeNanoseconds <<< unsafePartial fromJust <<< fromString $ "-86400000000000") (mkPreciseDateTime 1985 Date.March 13 0 0 0 0 0)
        `shouldEqual` (Just $ mkPreciseDateTime 1985 Date.March 12 0 0 0 0 0)

      adjust (PD.unsafeNanoseconds <<< unsafePartial fromJust <<< fromString $ "86400000000001") (mkPreciseDateTime 1985 Date.March 13 0 0 0 0 0)
        `shouldEqual` (Just $ mkPreciseDateTime 1985 Date.March 14 0 0 0 0 1)

      adjust (PD.unsafeNanoseconds <<< unsafePartial fromJust <<< fromString $ "-86400000000001") (mkPreciseDateTime 1985 Date.March 13 0 0 0 0 0)
        `shouldEqual` (Just $ mkPreciseDateTime 1985 Date.March 11 23 59 59 999 999999)

      adjust (PD.unsafeNanoseconds <<< unsafePartial fromJust <<< fromString $ "604800000000000") (mkPreciseDateTime 1985 Date.March 13 0 0 0 0 0)
        `shouldEqual` (Just $ mkPreciseDateTime 1985 Date.March 20 0 0 0 0 0)

      adjust (PD.unsafeNanoseconds <<< unsafePartial fromJust <<< fromString $ "-604800000000000") (mkPreciseDateTime 1985 Date.March 13 0 0 0 0 0)
        `shouldEqual` (Just $ mkPreciseDateTime 1985 Date.March 6 0 0 0 0 0)

      adjust (PD.unsafeNanoseconds <<< unsafePartial fromJust <<< fromString $ "604800000000001") (mkPreciseDateTime 1985 Date.March 13 0 0 0 0 0)
        `shouldEqual` (Just $ mkPreciseDateTime 1985 Date.March 20 0 0 0 0 1)

      adjust (PD.unsafeNanoseconds <<< unsafePartial fromJust <<< fromString $ "-604800000000001") (mkPreciseDateTime 1985 Date.March 13 0 0 0 0 0)
        `shouldEqual` (Just $ mkPreciseDateTime 1985 Date.March 5 23 59 59 999 999999)

      adjust (PD.unsafeNanoseconds <<< unsafePartial fromJust <<< fromString $ "-300000000000") (mkPreciseDateTime 2017 Date.September 17 0 0 0 0 0)
        `shouldEqual` (Just $ mkPreciseDateTime 2017 Date.September 16 23 55 0 0 0)

      adjust (PD.unsafeNanoseconds <<< unsafePartial fromJust <<< fromString $ "-300000000000") (mkPreciseDateTime 2017 Date.September 17 0 0 0 123 0)
        `shouldEqual` (Just $ mkPreciseDateTime 2017 Date.September 16 23 55 0 123 0)

      adjust (PD.unsafeNanoseconds <<< unsafePartial fromJust <<< fromString $ "-300000000000") (mkPreciseDateTime 2017 Date.September 17 0 0 0 123 456789)
        `shouldEqual` (Just $ mkPreciseDateTime 2017 Date.September 16 23 55 0 123 456789)
