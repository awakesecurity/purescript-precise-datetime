module Test.Data.Time.PreciseDuration.Spec where

import Prelude

import Control.Monad.Aff (Aff)
--import Data.BigInt (BigInt, fromString)
import Data.Decimal (Decimal)
import Data.Decimal as Decimal
import Data.Maybe (fromJust)
import Data.Time.PreciseDuration (PreciseDuration(..), day, hour, micro, milli, minute, nano, second, toDays, toHours, toMicroseconds, toMilliseconds, toMinutes, toNanoseconds, toSeconds, toWeeks, week)
import Partial.Unsafe (unsafePartial)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

unsafeFromString :: String -> Decimal
unsafeFromString = unsafePartial fromJust <<< Decimal.fromString

test
  :: forall r
   . (PreciseDuration -> PreciseDuration)
  -> (Decimal -> PreciseDuration)
  -> Decimal
  -> Decimal
  -> Aff r Unit
test fn ctr div input = do
--  fn (Nanoseconds input) `shouldEqual` (ctr $ input * nano / div)
  fn (Microseconds input) `shouldEqual` (ctr $ input * micro / div)
  fn (Milliseconds input) `shouldEqual` (ctr $ input * milli / div)
  fn (Seconds input) `shouldEqual` (ctr $ input * second / div)
  fn (Minutes input) `shouldEqual` (ctr $ input * minute / div)
  fn (Hours input) `shouldEqual` (ctr $ input * hour / div)
  fn (Weeks input) `shouldEqual` (ctr $ input * week / div)

spec :: forall r. Spec r Unit
spec =
  describe "PreciseDuration" do
    let input = unsafeFromString "123456789"

--    it "toNanoseconds" $ test toNanoseconds Nanoseconds nano input
    it "toMicroseconds" $ test toMicroseconds Microseconds micro input
    it "toMilliseconds" $ test toMilliseconds Milliseconds milli input
    it "toSeconds" $ test toSeconds Seconds second input
    it "toMinutes" $ test toMinutes Minutes minute input
    it "toHours" $ test toHours Hours hour input
    it "toDays" $ test toDays Days day input
    it "toWeeks" $ test toWeeks Weeks week input
