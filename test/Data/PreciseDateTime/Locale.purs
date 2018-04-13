module Test.Data.PreciseDateTime.Locale.Spec where

import Prelude

import Data.Decimal (fromInt)
import Data.DateTime.Locale (LocalValue(..), Locale(..))
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.PreciseDateTime (PreciseDateTime)
import Data.PreciseDateTime as PDT
import Data.PreciseDateTime.Locale (fromRFC3339String, toRFC3339String)
import Data.RFC3339String (RFC3339String(..))
import Data.Time.Duration as Dur
import Data.Time.PreciseDuration (PreciseDuration(..))
import Test.Data.PreciseDateTime.Spec (dateStringFixture, preciseDateTimeFixture)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

withTZ :: Int -> PreciseDateTime -> LocalValue PreciseDateTime
withTZ hrsTZ = LocalValue (Locale Nothing (Dur.convertDuration (Dur.Hours (toNumber hrsTZ))))
withTZMins :: Int -> PreciseDateTime -> LocalValue PreciseDateTime
withTZMins minsTZ = LocalValue (Locale Nothing (Dur.convertDuration (Dur.Minutes (toNumber minsTZ))))

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

      -- these tests are a bit finnicky because of how we normalise the RFC3339 representation,
      -- eg. an input offset of '+00:00' gets printed as 'Z'
      roundtrip (RFC3339String $ dateStringFixture <> ".0+08:00")

      roundtrip (RFC3339String $ dateStringFixture <> ".0-08:00")

      roundtrip (RFC3339String $ dateStringFixture <> ".0Z")

      roundtrip (RFC3339String $ dateStringFixture <> ".0-00:01")
