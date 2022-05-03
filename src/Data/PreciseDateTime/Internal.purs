module Data.PreciseDateTime.Internal where

import Prelude

import Data.Array as Array
import Data.Formatter.DateTime (FormatterCommand(..))
import Data.List (List, fromFoldable)
import Data.String.CodeUnits as String
import Data.Tuple (Tuple(..), snd)

dateFormat :: List FormatterCommand
dateFormat = fromFoldable
  [ YearFull
  , Placeholder "-"
  , MonthTwoDigits
  , Placeholder "-"
  , DayOfMonthTwoDigits
  ]

timeFormat :: List FormatterCommand
timeFormat = fromFoldable
  [ Hours24
  , Placeholder ":"
  , MinutesTwoDigits
  , Placeholder ":"
  , SecondsTwoDigits
  ]

dateTimeFormatISO :: List FormatterCommand
dateTimeFormatISO = dateFormat <> pure (Placeholder "T") <> timeFormat

-- | Returns the prefix remaining after dropping characters that satisfy the
-- | predicate from the end of the string.
dropWhileEnd :: (Char -> Boolean) -> String -> String
dropWhileEnd p s = snd $ Array.foldr check (Tuple false "") (String.toCharArray s)
  where
  check c state@(Tuple false _) = if p c then state else Tuple true (String.singleton c)
  check c (Tuple true string) = Tuple true (String.singleton c <> string)
