module Test.Data.Time.PreciseDuration.Format.Spec where

import Prelude

import Data.Decimal (Decimal)
import Data.Decimal as Decimal
import Data.Time.PreciseDuration (PreciseDuration, day, hour, micro, milli, minute, nano, second, week)
import Data.Time.PreciseDuration as PD
import Data.Time.PreciseDuration.Format (Format, formatPreciseDuration')
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)

test' :: Format -> PreciseDuration -> String -> Spec Unit
test' format dur expected = do
  describe (expected <> ", -" <> expected <> " " <> show format) do
    it ("should format " <> expected) do
      formatPreciseDuration' format dur `shouldEqual` expected

    when (dur /= PD.nanoseconds zero) $ it ("should format -" <> expected) do
      formatPreciseDuration' format (PD.negatePreciseDuration dur) `shouldEqual` ("-" <> expected)

testWz :: PreciseDuration -> String -> Spec Unit
testWz = test' { intermediateZeroes: true }

testWoZ :: PreciseDuration -> String -> Spec Unit
testWoZ = test' { intermediateZeroes: false }

test :: PreciseDuration -> String -> Spec Unit
test dur expected = do
  testWz dur expected
  testWoZ dur expected

two :: Decimal
two = Decimal.fromNumber 2.0

spec :: Spec Unit
spec = describe "Data.Time.PreciseDuration.Format" do
  describe "formatPreciseDuration" do
    describe "zero" do
      test (PD.nanoseconds zero) "0s"
      test (PD.nanoseconds $ -zero) "0s"

    describe "nanoseconds" do
      test (PD.unsafeNanoseconds nano) "0.000000001s"
      test (PD.unsafeNanoseconds $ nano * two) "0.000000002s"
      test (PD.unsafeNanoseconds $ micro - nano) "0.000000999s"

    describe "microseconds" do
      test (PD.unsafeNanoseconds micro) "0.000001s"
      test (PD.unsafeNanoseconds $ micro * two) "0.000002s"
      test (PD.unsafeNanoseconds $ micro + nano) "0.000001001s"
      test (PD.unsafeNanoseconds $ milli - micro) "0.000999s"
      test (PD.unsafeNanoseconds $ milli - nano) "0.000999999s"
      test (PD.unsafeNanoseconds $ milli - micro - nano) "0.000998999s"

    describe "milliseconds" do
      test (PD.unsafeNanoseconds milli) "0.001s"
      test (PD.unsafeNanoseconds $ milli * two) "0.002s"
      test (PD.unsafeNanoseconds $ milli + micro) "0.001001s"
      test (PD.unsafeNanoseconds $ milli + nano) "0.001000001s"
      test (PD.unsafeNanoseconds $ milli + micro + nano) "0.001001001s"

    describe "seconds" do
      test (PD.unsafeNanoseconds second) "1s"
      test (PD.unsafeNanoseconds $ second * two) "2s"

      test (PD.unsafeNanoseconds $ second + milli) "1.001s"
      test (PD.unsafeNanoseconds $ second + micro) "1.000001s"
      test (PD.unsafeNanoseconds $ second + nano) "1.000000001s"

      test (PD.unsafeNanoseconds $ minute - second) "59s"
      test (PD.unsafeNanoseconds $ minute - milli) "59.999s"
      test (PD.unsafeNanoseconds $ minute - micro) "59.999999s"
      test (PD.unsafeNanoseconds $ minute - nano) "59.999999999s"

      test (PD.unsafeNanoseconds $ minute - second - milli) "58.999s"
      test (PD.unsafeNanoseconds $ minute - second - micro) "58.999999s"
      test (PD.unsafeNanoseconds $ minute - second - nano) "58.999999999s"

    describe "minutes" do
      test (PD.unsafeNanoseconds minute) "1m"
      test (PD.unsafeNanoseconds $ minute * two) "2m"

      test (PD.unsafeNanoseconds $ minute + second) "1m 1s"
      test (PD.unsafeNanoseconds $ minute + milli) "1m 0.001s"
      test (PD.unsafeNanoseconds $ minute + micro) "1m 0.000001s"
      test (PD.unsafeNanoseconds $ minute + nano) "1m 0.000000001s"

      test (PD.unsafeNanoseconds $ minute + second + milli) "1m 1.001s"
      test (PD.unsafeNanoseconds $ minute + second + micro) "1m 1.000001s"
      test (PD.unsafeNanoseconds $ minute + second + nano) "1m 1.000000001s"

      test (PD.unsafeNanoseconds $ hour - minute) "59m"
      test (PD.unsafeNanoseconds $ hour - second) "59m 59s"
      test (PD.unsafeNanoseconds $ hour - milli) "59m 59.999s"
      test (PD.unsafeNanoseconds $ hour - micro) "59m 59.999999s"
      test (PD.unsafeNanoseconds $ hour - nano) "59m 59.999999999s"

      test (PD.unsafeNanoseconds $ hour - minute - second) "58m 59s"
      test (PD.unsafeNanoseconds $ hour - minute - second - milli) "58m 58.999s"
      test (PD.unsafeNanoseconds $ hour - minute - second - micro) "58m 58.999999s"
      test (PD.unsafeNanoseconds $ hour - minute - second - nano) "58m 58.999999999s"

    describe "hours" do
      test (PD.unsafeNanoseconds hour) "1h"
      test (PD.unsafeNanoseconds $ hour * two) "2h"

      test (PD.unsafeNanoseconds $ hour + minute) "1h 1m"

      testWz (PD.unsafeNanoseconds $ hour + second) "1h 0m 1s"
      testWz (PD.unsafeNanoseconds $ hour + milli) "1h 0m 0.001s"
      testWz (PD.unsafeNanoseconds $ hour + micro) "1h 0m 0.000001s"
      testWz (PD.unsafeNanoseconds $ hour + nano) "1h 0m 0.000000001s"

      testWoZ (PD.unsafeNanoseconds $ hour + second) "1h 1s"
      testWoZ (PD.unsafeNanoseconds $ hour + milli) "1h 0.001s"
      testWoZ (PD.unsafeNanoseconds $ hour + micro) "1h 0.000001s"
      testWoZ (PD.unsafeNanoseconds $ hour + nano) "1h 0.000000001s"

      test (PD.unsafeNanoseconds $ hour + minute + second) "1h 1m 1s"
      test (PD.unsafeNanoseconds $ hour + minute + milli) "1h 1m 0.001s"
      test (PD.unsafeNanoseconds $ hour + minute + micro) "1h 1m 0.000001s"
      test (PD.unsafeNanoseconds $ hour + minute + nano) "1h 1m 0.000000001s"

      test (PD.unsafeNanoseconds $ day - hour) "23h"
      test (PD.unsafeNanoseconds $ day - minute) "23h 59m"
      test (PD.unsafeNanoseconds $ day - second) "23h 59m 59s"
      test (PD.unsafeNanoseconds $ day - milli) "23h 59m 59.999s"
      test (PD.unsafeNanoseconds $ day - micro) "23h 59m 59.999999s"
      test (PD.unsafeNanoseconds $ day - nano) "23h 59m 59.999999999s"

      test (PD.unsafeNanoseconds $ day - hour - minute) "22h 59m"
      test (PD.unsafeNanoseconds $ day - hour - second) "22h 59m 59s"
      test (PD.unsafeNanoseconds $ day - hour - milli) "22h 59m 59.999s"
      test (PD.unsafeNanoseconds $ day - hour - micro) "22h 59m 59.999999s"
      test (PD.unsafeNanoseconds $ day - hour - nano) "22h 59m 59.999999999s"

    describe "days" do
      test (PD.unsafeNanoseconds day) "1d"
      test (PD.unsafeNanoseconds $ day * two) "2d"

      test (PD.unsafeNanoseconds $ day + hour) "1d 1h"

      testWz (PD.unsafeNanoseconds $ day + minute) "1d 0h 1m"
      testWz (PD.unsafeNanoseconds $ day + second) "1d 0h 0m 1s"
      testWz (PD.unsafeNanoseconds $ day + milli) "1d 0h 0m 0.001s"
      testWz (PD.unsafeNanoseconds $ day + micro) "1d 0h 0m 0.000001s"
      testWz (PD.unsafeNanoseconds $ day + nano) "1d 0h 0m 0.000000001s"

      testWoZ (PD.unsafeNanoseconds $ day + minute) "1d 1m"
      testWoZ (PD.unsafeNanoseconds $ day + second) "1d 1s"
      testWoZ (PD.unsafeNanoseconds $ day + milli) "1d 0.001s"
      testWoZ (PD.unsafeNanoseconds $ day + micro) "1d 0.000001s"
      testWoZ (PD.unsafeNanoseconds $ day + nano) "1d 0.000000001s"

      test (PD.unsafeNanoseconds $ day + hour + minute) "1d 1h 1m"

      testWz (PD.unsafeNanoseconds $ day + hour + second) "1d 1h 0m 1s"
      testWz (PD.unsafeNanoseconds $ day + hour + milli) "1d 1h 0m 0.001s"
      testWz (PD.unsafeNanoseconds $ day + hour + micro) "1d 1h 0m 0.000001s"
      testWz (PD.unsafeNanoseconds $ day + hour + nano) "1d 1h 0m 0.000000001s"

      testWoZ (PD.unsafeNanoseconds $ day + hour + second) "1d 1h 1s"
      testWoZ (PD.unsafeNanoseconds $ day + hour + milli) "1d 1h 0.001s"
      testWoZ (PD.unsafeNanoseconds $ day + hour + micro) "1d 1h 0.000001s"
      testWoZ (PD.unsafeNanoseconds $ day + hour + nano) "1d 1h 0.000000001s"

      test (PD.unsafeNanoseconds $ week - day) "6d"
      test (PD.unsafeNanoseconds $ week - hour) "6d 23h"
      test (PD.unsafeNanoseconds $ week - minute) "6d 23h 59m"
      test (PD.unsafeNanoseconds $ week - second) "6d 23h 59m 59s"
      test (PD.unsafeNanoseconds $ week - milli) "6d 23h 59m 59.999s"
      test (PD.unsafeNanoseconds $ week - micro) "6d 23h 59m 59.999999s"
      test (PD.unsafeNanoseconds $ week - nano) "6d 23h 59m 59.999999999s"

      test (PD.unsafeNanoseconds $ week - day - hour) "5d 23h"
      test (PD.unsafeNanoseconds $ week - day - minute) "5d 23h 59m"
      test (PD.unsafeNanoseconds $ week - day - second) "5d 23h 59m 59s"
      test (PD.unsafeNanoseconds $ week - day - milli) "5d 23h 59m 59.999s"
      test (PD.unsafeNanoseconds $ week - day - micro) "5d 23h 59m 59.999999s"
      test (PD.unsafeNanoseconds $ week - day - nano) "5d 23h 59m 59.999999999s"

    describe "week" do
      test (PD.unsafeNanoseconds week) "1w"
      test (PD.unsafeNanoseconds $ week * two) "2w"

      test (PD.unsafeNanoseconds $ week + day) "1w 1d"

      testWz (PD.unsafeNanoseconds $ week + hour) "1w 0d 1h"
      testWz (PD.unsafeNanoseconds $ week + minute) "1w 0d 0h 1m"
      testWz (PD.unsafeNanoseconds $ week + second) "1w 0d 0h 0m 1s"
      testWz (PD.unsafeNanoseconds $ week + milli) "1w 0d 0h 0m 0.001s"
      testWz (PD.unsafeNanoseconds $ week + micro) "1w 0d 0h 0m 0.000001s"
      testWz (PD.unsafeNanoseconds $ week + nano) "1w 0d 0h 0m 0.000000001s"

      testWoZ (PD.unsafeNanoseconds $ week + hour) "1w 1h"
      testWoZ (PD.unsafeNanoseconds $ week + minute) "1w 1m"
      testWoZ (PD.unsafeNanoseconds $ week + second) "1w 1s"
      testWoZ (PD.unsafeNanoseconds $ week + milli) "1w 0.001s"
      testWoZ (PD.unsafeNanoseconds $ week + micro) "1w 0.000001s"
      testWoZ (PD.unsafeNanoseconds $ week + nano) "1w 0.000000001s"

      test (PD.unsafeNanoseconds $ week + day + hour) "1w 1d 1h"

      testWz (PD.unsafeNanoseconds $ week + day + minute) "1w 1d 0h 1m"
      testWz (PD.unsafeNanoseconds $ week + day + second) "1w 1d 0h 0m 1s"
      testWz (PD.unsafeNanoseconds $ week + day + milli) "1w 1d 0h 0m 0.001s"
      testWz (PD.unsafeNanoseconds $ week + day + micro) "1w 1d 0h 0m 0.000001s"
      testWz (PD.unsafeNanoseconds $ week + day + nano) "1w 1d 0h 0m 0.000000001s"

      testWoZ (PD.unsafeNanoseconds $ week + day + minute) "1w 1d 1m"
      testWoZ (PD.unsafeNanoseconds $ week + day + second) "1w 1d 1s"
      testWoZ (PD.unsafeNanoseconds $ week + day + milli) "1w 1d 0.001s"
      testWoZ (PD.unsafeNanoseconds $ week + day + micro) "1w 1d 0.000001s"
      testWoZ (PD.unsafeNanoseconds $ week + day + nano) "1w 1d 0.000000001s"

      test (PD.unsafeNanoseconds $ (week * two) - day) "1w 6d"
      test (PD.unsafeNanoseconds $ (week * two) - hour) "1w 6d 23h"
      test (PD.unsafeNanoseconds $ (week * two) - minute) "1w 6d 23h 59m"
      test (PD.unsafeNanoseconds $ (week * two) - second) "1w 6d 23h 59m 59s"
      test (PD.unsafeNanoseconds $ (week * two) - milli) "1w 6d 23h 59m 59.999s"
      test (PD.unsafeNanoseconds $ (week * two) - micro) "1w 6d 23h 59m 59.999999s"
      test (PD.unsafeNanoseconds $ (week * two) - nano) "1w 6d 23h 59m 59.999999999s"

      test (PD.unsafeNanoseconds $ (week * two) - day - hour) "1w 5d 23h"
      test (PD.unsafeNanoseconds $ (week * two) - day - minute) "1w 5d 23h 59m"
      test (PD.unsafeNanoseconds $ (week * two) - day - second) "1w 5d 23h 59m 59s"
      test (PD.unsafeNanoseconds $ (week * two) - day - milli) "1w 5d 23h 59m 59.999s"
      test (PD.unsafeNanoseconds $ (week * two) - day - micro) "1w 5d 23h 59m 59.999999s"
      test (PD.unsafeNanoseconds $ (week * two) - day - nano) "1w 5d 23h 59m 59.999999999s"
