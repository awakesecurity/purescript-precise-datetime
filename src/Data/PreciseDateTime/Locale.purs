module Data.PreciseDateTime.Locale where

import Prelude

import Data.DateTime.Locale (LocalValue(..), LocalDateTime)
import Data.Maybe (Maybe)
import Data.PreciseDateTime (PreciseDateTime)
import Data.PreciseDateTime as PDT
import Data.RFC3339String (RFC3339String(..))
import Data.RFC3339String as RFC3339String
import Data.RFC3339String.Format (formatLocale)
import Data.String (dropRight)
import Data.Time.PreciseDuration (PreciseDuration)
import Data.Traversable (traverse)

type LocalPreciseDateTime = LocalValue PreciseDateTime

adjust :: PreciseDuration -> LocalPreciseDateTime -> Maybe LocalPreciseDateTime
adjust = traverse <<< PDT.adjust

fromRFC3339String :: RFC3339String -> Maybe LocalPreciseDateTime
fromRFC3339String = do
  loc <- RFC3339String.toLocale
  pdt <- PDT.fromRFC3339String
  pure $ LocalValue loc <$> pdt

toRFC3339String :: LocalPreciseDateTime -> RFC3339String
toRFC3339String (LocalValue locale pdt) =
  let
    (RFC3339String s) = PDT.toRFC3339String pdt
  in
    RFC3339String $ dropRight 1 s <> formatLocale locale

toLocalDateTimeLossy :: LocalPreciseDateTime -> LocalDateTime
toLocalDateTimeLossy = map PDT.toDateTimeLossy

fromLocalDateTime :: LocalDateTime -> LocalPreciseDateTime
fromLocalDateTime = map PDT.fromDateTime
