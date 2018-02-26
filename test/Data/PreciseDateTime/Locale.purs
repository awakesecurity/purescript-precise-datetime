module Test.Data.PreciseDateTime.Locale.Spec where

import Prelude

import Data.BigInt (fromInt)
import Data.DateTime.Locale (LocalValue(..), Locale(..))
import Data.Int (toNumber)
import Data.Maybe (Maybe(..))
import Data.PreciseDateTime as PDT
import Data.PreciseDateTime.Locale (fromRFC3339String)
import Data.RFC3339String (RFC3339String(..))
import Data.Time.Duration as Dur
import Data.Time.PreciseDuration (PreciseDuration(..))
import Test.Data.PreciseDateTime.Spec (dateStringFixture, preciseDateTimeFixture)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

spec :: forall r. Spec r Unit
spec =
  describe "LocalPreciseDateTime" do
    it "locale" do
      let withTZ hrsTZ = map (LocalValue (Locale Nothing (Dur.convertDuration (Dur.Hours (toNumber hrsTZ)))))
                         <<< PDT.adjust (Hours (fromInt (negate hrsTZ)))

      fromRFC3339String (RFC3339String $ dateStringFixture <> "+08:00")
        `shouldEqual` withTZ 8 (preciseDateTimeFixture 0 0)

      fromRFC3339String (RFC3339String $ dateStringFixture <> "-08:00")
        `shouldEqual` withTZ (-8) (preciseDateTimeFixture 0 0)
