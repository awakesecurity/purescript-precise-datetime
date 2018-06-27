module Test.Awake.RFC3339String.Spec where

import Prelude

import Data.Date as Date
import Data.RFC3339String (RFC3339String(..), fromDateTime)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Utils (mkDateTime)

spec :: Spec Unit
spec =
  describe "RFC3339String" do
    it "fromDateTime" do
      fromDateTime (mkDateTime 1985 Date.March 13 12 34 56 0)
        `shouldEqual` (RFC3339String "1985-03-13T12:34:56.0Z")

      fromDateTime (mkDateTime 1985 Date.March 13 12 34 56 1)
        `shouldEqual` (RFC3339String "1985-03-13T12:34:56.001Z")

      fromDateTime (mkDateTime 1985 Date.March 13 12 34 56 10)
        `shouldEqual` (RFC3339String "1985-03-13T12:34:56.01Z")

      fromDateTime (mkDateTime 1985 Date.March 13 12 34 56 100)
        `shouldEqual` (RFC3339String "1985-03-13T12:34:56.1Z")

      fromDateTime (mkDateTime 1985 Date.March 13 12 34 56 9)
        `shouldEqual` (RFC3339String "1985-03-13T12:34:56.009Z")

      fromDateTime (mkDateTime 1985 Date.March 13 12 34 56 89)
        `shouldEqual` (RFC3339String "1985-03-13T12:34:56.089Z")

      fromDateTime (mkDateTime 1985 Date.March 13 12 34 56 789)
        `shouldEqual` (RFC3339String "1985-03-13T12:34:56.789Z")
