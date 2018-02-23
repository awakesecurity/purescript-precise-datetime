module Data.RFC3339String.Format where

import Prelude

import Data.DateTime.Locale (Locale(..))
import Data.Formatter.DateTime (FormatterCommand(Placeholder, Milliseconds))
import Data.Int (floor)
import Data.List (List, fromFoldable)
import Data.PreciseDateTime.Internal (dateTimeFormatISO)
import Data.String (takeRight)
import Data.Time.Duration (Minutes(..))
import Math (abs)

iso8601Format :: List FormatterCommand
iso8601Format = dateTimeFormatISO <> fromFoldable
  [ Placeholder "."
  , Milliseconds
  , Placeholder "Z"
  ]

-- Assumes the locale is valid, i.e. the offset is between
-- [-720, +720] minutes.
formatLocale :: Locale -> String
formatLocale (Locale _ (Minutes mins))
  | mins == zero = "Z"
  | otherwise = s <> padShow (m `div` 60) <>":" <> padShow (m `mod` 60)
  where
    padShow n = takeRight 2 $ "00" <> show n
    s | mins >= zero = "+"
      | otherwise    = "-"
    m = floor <<< abs $ mins
