module Test.Data.Time.PreciseDuration.Spec where

import Data.Time.PreciseDuration (PreciseDuration(..), toDays, toHours, toMicroseconds, toMilliseconds, toMinutes, toNanoseconds, toSeconds, toWeeks)
import Data.Time.PreciseDuration as PD
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
test fn ctr div = traverse_ \input -> do
  -- do not feed fractional values into nanoseconds
  when (Decimal.isInteger input) $
    fn (PD.unsafeNanoseconds input) `shouldEqual` (ctr $ input * nano / div)
  fn (PD.microseconds input) `shouldEqual` (ctr $ input * micro / div)
  fn (PD.milliseconds input) `shouldEqual` (ctr $ input * milli / div)
  fn (PD.seconds input) `shouldEqual` (ctr $ input * second / div)
  fn (PD.minutes input) `shouldEqual` (ctr $ input * minute / div)
  fn (PD.hours input) `shouldEqual` (ctr $ input * hour / div)
  fn (PD.weeks input) `shouldEqual` (ctr $ input * week / div)

spec :: forall r. Spec r Unit
spec =
  describe "PreciseDuration" do
    let inputs = [ unsafeFromString "123456789", unsafeFromString "0.5" ]

    it "toNanoseconds" $ test toNanoseconds PD.unsafeNanoseconds nano inputs
    it "toMicroseconds" $ test toMicroseconds PD.microseconds micro inputs
    it "toMilliseconds" $ test toMilliseconds PD.milliseconds milli inputs
    it "toSeconds" $ test toSeconds PD.seconds second inputs
    it "toMinutes" $ test toMinutes PD.minutes minute inputs
    it "toHours" $ test toHours PD.hours hour inputs
    it "toDays" $ test toDays PD.days day inputs
    it "toWeeks" $ test toWeeks PD.weeks week inputs
