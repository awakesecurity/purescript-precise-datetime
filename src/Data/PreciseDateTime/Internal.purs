module Data.PreciseDateTime.Internal where

import Prelude

import Data.Formatter.DateTime (FormatterCommand(..))
import Data.List (List, fromFoldable)

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
