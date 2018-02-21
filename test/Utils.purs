module Test.Utils where

import Prelude

import Data.Date as Date
import Data.DateTime (DateTime(..))
import Data.Enum (toEnum)
import Data.Maybe (fromJust)
import Data.Time (Time(..))
import Partial.Unsafe (unsafePartial)

mkDateTime :: Int -> Date.Month -> Int -> Int -> Int -> Int -> Int -> DateTime
mkDateTime yyyy month dd hh mm ss ms =
  DateTime
    (unsafePartial fromJust $ Date.canonicalDate <$> toEnum yyyy <*> pure month <*> toEnum dd)
    (unsafePartial fromJust $ Time <$> toEnum hh <*> toEnum mm <*> toEnum ss <*> toEnum ms)
