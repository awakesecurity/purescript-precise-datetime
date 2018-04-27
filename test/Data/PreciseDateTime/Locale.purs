module Test.Data.PreciseDateTime.Locale.Spec where

import Prelude

import Data.Decimal (Decimal)
import Data.Decimal as Decimal
import Data.Date as Date
import Data.DateTime.Locale (LocalValue(..), Locale(..))
import Data.Int (toNumber, floor)
import Data.Maybe (Maybe(..), fromJust)
import Data.PreciseDateTime (PreciseDateTime)
import Data.PreciseDateTime as PDT
import Data.PreciseDateTime.Locale (fromRFC3339String, toRFC3339String, diff)
import Data.RFC3339String (RFC3339String(..))
import Data.Time.Duration as Dur
import Data.Time.PreciseDuration as PD
import Partial.Unsafe (unsafePartial)
import Test.Data.PreciseDateTime.Spec (dateStringFixture, preciseDateTimeFixture, mkPreciseDateTime)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

withTZ :: Int -> PreciseDateTime -> LocalValue PreciseDateTime
withTZ hrsTZ = LocalValue (Locale Nothing (Dur.convertDuration (Dur.Hours (toNumber hrsTZ))))

withTZMins :: Int -> PreciseDateTime -> LocalValue PreciseDateTime
withTZMins = withTZMinsNum <<< toNumber

withTZMinsNum :: Number -> PreciseDateTime -> LocalValue PreciseDateTime
withTZMinsNum minsTZ = LocalValue (Locale Nothing (Dur.convertDuration (Dur.Minutes minsTZ)))

unsafeFromString :: String -> Decimal
unsafeFromString s = unsafePartial $ fromJust (Decimal.fromString s)

minsToNs :: Int -> Decimal
minsToNs m = Decimal.fromInt m * unsafeFromString "6E10"

spec :: forall r. Spec r Unit
spec =
  describe "LocalPreciseDateTime" do
    it "fromRFC3339String" do

      fromRFC3339String (RFC3339String $ dateStringFixture <> "+08:00")
        `shouldEqual` Just (withTZ 8 (preciseDateTimeFixture 0 0))

      fromRFC3339String (RFC3339String $ dateStringFixture <> "-08:00")
        `shouldEqual` Just (withTZ (-8) (preciseDateTimeFixture 0 0))

      fromRFC3339String (RFC3339String $ dateStringFixture <> "Z")
        `shouldEqual` Just (withTZ 0 (preciseDateTimeFixture 0 0))

      fromRFC3339String (RFC3339String $ dateStringFixture <> "-00:00")
        `shouldEqual` Just (withTZ 0 (preciseDateTimeFixture 0 0))

      fromRFC3339String (RFC3339String $ dateStringFixture <> "+00:00")
        `shouldEqual` Just (withTZ 0 (preciseDateTimeFixture 0 0))

      fromRFC3339String (RFC3339String $ dateStringFixture <> "-00:01")
        `shouldEqual` Just (withTZMins (-1) (preciseDateTimeFixture 0 0))

    it "toRFC3339String" do
      toRFC3339String (LocalValue (Locale Nothing zero) (preciseDateTimeFixture 0 0))
        `shouldEqual` RFC3339String (dateStringFixture <>".0Z")

      toRFC3339String (LocalValue (Locale Nothing (Dur.convertDuration (Dur.Hours 4.0))) (preciseDateTimeFixture 0 0))
        `shouldEqual` RFC3339String (dateStringFixture <> ".0+04:00")

    it "Round Trip RFC3339String" do
      let roundtrip rfcStr = let go = map toRFC3339String <<< fromRFC3339String
                             in (Just rfcStr) `shouldEqual` go rfcStr

      -- These tests are a bit finnicky because of how we normalise the RFC3339 representation,
      -- eg. an input offset of '+00:00' gets printed as 'Z'
      roundtrip (RFC3339String $ dateStringFixture <> ".0+08:00")

      roundtrip (RFC3339String $ dateStringFixture <> ".0-08:00")

      roundtrip (RFC3339String $ dateStringFixture <> ".0Z")

      roundtrip (RFC3339String $ dateStringFixture <> ".0-00:01")

    -- The diff machinery is tested in the 'PreciseDateTime' suite, here we only
    -- test locale diffing.
    it "diff" do
      diff (withTZMins 0 (mkPreciseDateTime 1985 Date.March 13 0 0 0 0 0))
           (withTZMins 0 (mkPreciseDateTime 1985 Date.March 13 0 0 0 0 0))
        `shouldEqual` (PD.unsafeNanoseconds zero)

      diff (withTZMins 100 (mkPreciseDateTime 1985 Date.March 13 0 0 0 0 0))
           (withTZMins 100 (mkPreciseDateTime 1985 Date.March 13 0 0 0 0 0))
        `shouldEqual` (PD.unsafeNanoseconds zero)

      diff (withTZMins 100 (mkPreciseDateTime 1985 Date.March 13 0 0 0 0 0))
           (withTZMins (-100) (mkPreciseDateTime 1985 Date.March 13 0 0 0 0 0))
        `shouldEqual` (PD.unsafeNanoseconds (minsToNs 200))

      diff (withTZMinsNum 0.01 (mkPreciseDateTime 1985 Date.March 13 0 0 0 0 0))
           (withTZMins 0 (mkPreciseDateTime 1985 Date.March 12 23 59 59 999 999999))
        `shouldEqual` (PD.unsafeNanoseconds (Decimal.fromInt 1 + unsafeFromString "6E8"))

      diff (withTZMins 456 (mkPreciseDateTime 1985 Date.March 13 0 0 0 0 0))
           (withTZMins 123 (mkPreciseDateTime 1985 Date.March 12 23 59 58 999 999999))
        `shouldEqual` (PD.unsafeNanoseconds (Decimal.fromInt 1000000001 + minsToNs (456 - 123)))
