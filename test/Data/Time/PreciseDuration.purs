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
test fn ctr div = traverse_ \(input :: Decimal) -> do
  -- do not feed fractional values into nanoseconds
  when (Decimal.isInteger input) $
    fn (PD.unsafeNanoseconds input) `shouldEqual` (ctr $ input * nano / div)
  fn (PD.make.microseconds input) `shouldEqual` (ctr $ input * micro / div)
  fn (PD.make.milliseconds input) `shouldEqual` (ctr $ input * milli / div)
  fn (PD.make.seconds input) `shouldEqual` (ctr $ input * second / div)
  fn (PD.make.minutes input) `shouldEqual` (ctr $ input * minute / div)
  fn (PD.make.hours input) `shouldEqual` (ctr $ input * hour / div)
  fn (PD.make.weeks input) `shouldEqual` (ctr $ input * week / div)

spec :: forall r. Spec r Unit
spec =
  describe "PreciseDuration" do
    let inputs = [ unsafeFromString "123456789", unsafeFromString "0.5" ]

    it "toNanoseconds" $ test toNanoseconds PD.unsafeNanoseconds nano inputs
    it "toMicroseconds" $ test toMicroseconds PD.make.microseconds micro inputs
    it "toMilliseconds" $ test toMilliseconds PD.make.milliseconds milli inputs
    it "toSeconds" $ test toSeconds PD.make.seconds second inputs
    it "toMinutes" $ test toMinutes PD.make.minutes minute inputs
    it "toHours" $ test toHours PD.make.hours hour inputs
    it "toDays" $ test toDays PD.make.days day inputs
    it "toWeeks" $ test toWeeks PD.make.weeks week inputs
