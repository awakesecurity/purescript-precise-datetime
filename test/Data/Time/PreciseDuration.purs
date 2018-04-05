module Test.Data.Time.PreciseDuration.Spec where

import Data.Time.PreciseDuration (PreciseDuration(..), toDays, toHours, toMicroseconds, toMilliseconds, toMinutes, toNanoseconds, toSeconds, toWeeks)
import Data.Time.PreciseDuration.Internal (day, hour, micro, milli, minute, nano, second, week)
import Prelude (Unit, discard, when, ($), (*), (/), (<<<))
import Control.Monad.Aff (Aff)
import Data.Decimal (Decimal)
import Data.Decimal as Decimal
import Data.Maybe (fromJust)
import Data.Traversable (traverse_)
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
  -> Array Decimal
  -> Aff r Unit
test fn ctr div = traverse_ \(input :: Decimal) -> do
  -- do not feed fractional values into nanoseconds
  when (Decimal.isInteger input) $
    fn (Nanoseconds input) `shouldEqual` (ctr $ input * nano / div)
  fn (Microseconds input) `shouldEqual` (ctr $ input * micro / div)
  fn (Milliseconds input) `shouldEqual` (ctr $ input * milli / div)
  fn (Seconds input) `shouldEqual` (ctr $ input * second / div)
  fn (Minutes input) `shouldEqual` (ctr $ input * minute / div)
  fn (Hours input) `shouldEqual` (ctr $ input * hour / div)
  fn (Weeks input) `shouldEqual` (ctr $ input * week / div)

spec :: forall r. Spec r Unit
spec =
  describe "PreciseDuration" do
    let inputs = [ unsafeFromString "123456789", unsafeFromString "0.5" ]

    it "toNanoseconds" $ test toNanoseconds Nanoseconds nano inputs
    it "toMicroseconds" $ test toMicroseconds Microseconds micro inputs
    it "toMilliseconds" $ test toMilliseconds Milliseconds milli inputs
    it "toSeconds" $ test toSeconds Seconds second inputs
    it "toMinutes" $ test toMinutes Minutes minute inputs
    it "toHours" $ test toHours Hours hour inputs
    it "toDays" $ test toDays Days day inputs
    it "toWeeks" $ test toWeeks Weeks week inputs
