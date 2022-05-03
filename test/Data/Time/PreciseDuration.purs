module Test.Data.Time.PreciseDuration.Spec where

import Prelude

import Data.Decimal (Decimal)
import Data.Decimal as Decimal
import Data.Maybe (fromJust)
import Data.Time.PreciseDuration (PreciseDuration, day, hour, micro, milli, minute, nano, second, toDays, toHours, toMicroseconds, toMilliseconds, toMinutes, toNanoseconds, toSeconds, toWeeks, week)
import Data.Time.PreciseDuration as PD
import Data.Traversable (traverse_)
import Effect.Aff (Aff)
import Partial.Unsafe (unsafePartial)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

unsafeFromString :: String -> Decimal
unsafeFromString = unsafePartial fromJust <<< Decimal.fromString

test
  :: (PreciseDuration -> PreciseDuration)
  -> (Decimal -> PreciseDuration)
  -> Decimal
  -> Array Decimal
  -> Aff Unit
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

spec :: Spec Unit
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

    it "toString" do
      PD.toString (PD.nanoseconds 1) `shouldEqual` "1ns"
      PD.toString (PD.microseconds $ Decimal.fromNumber 1.0) `shouldEqual` "1us"
      PD.toString (PD.milliseconds $ Decimal.fromNumber 1.0) `shouldEqual` "1ms"
      PD.toString (PD.seconds $ Decimal.fromNumber 1.0) `shouldEqual` "1s"
      PD.toString (PD.minutes $ Decimal.fromNumber 1.0) `shouldEqual` "1m"
      PD.toString (PD.hours $ Decimal.fromNumber 1.0) `shouldEqual` "1h"
      PD.toString (PD.days $ Decimal.fromNumber 1.0) `shouldEqual` "1d"
      PD.toString (PD.weeks $ Decimal.fromNumber 1.0) `shouldEqual` "1w"

      -- Test that exponential notation is not used
      PD.toString (PD.toMicroseconds $ PD.nanoseconds 1) `shouldEqual` "0.001us"
      PD.toString (PD.toMilliseconds $ PD.nanoseconds 1) `shouldEqual` "0.000001ms"
      PD.toString (PD.toSeconds $ PD.nanoseconds 1) `shouldEqual` "0.000000001s"
      PD.toString (PD.toMinutes $ PD.nanoseconds 1) `shouldEqual` "0.00000000001666666667m"
      PD.toString (PD.toHours $ PD.nanoseconds 1) `shouldEqual` "0.00000000000027777778h"
      PD.toString (PD.toDays $ PD.nanoseconds 1) `shouldEqual` "0.00000000000001157407d"
      PD.toString (PD.toWeeks $ PD.nanoseconds 1) `shouldEqual` "0.00000000000000165344w"
