module Data.RFC3339String.Format where

import Prelude

import Data.Formatter.DateTime (FormatterCommand(Placeholder, Milliseconds))
import Data.List (List, fromFoldable)
import Data.PreciseDateTime.Internal (dateTimeFormatISO)

iso8601Format :: List FormatterCommand
iso8601Format = dateTimeFormatISO <> fromFoldable
  [ Placeholder "."
  , Milliseconds
  , Placeholder "Z"
  ]
