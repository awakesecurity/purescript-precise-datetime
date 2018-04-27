module Data.PreciseDateTime.Locale where

import Prelude

import Data.DateTime.Locale (Locale(..), LocalValue(..), LocalDateTime)
import Data.Decimal as Decimal
import Data.Maybe (Maybe)
import Data.Newtype (unwrap)
import Data.PreciseDateTime (PreciseDateTime)
import Data.PreciseDateTime as PDT
import Data.RFC3339String (RFC3339String(..))
import Data.RFC3339String as RFC3339String
import Data.RFC3339String.Format (formatLocale)
import Data.String (dropRight)
import Data.Time.PreciseDuration (PreciseDuration)
import Data.Time.PreciseDuration as PD
import Data.Traversable (traverse)

type LocalPreciseDateTime = LocalValue PreciseDateTime

adjust :: PreciseDuration -> LocalPreciseDateTime -> Maybe LocalPreciseDateTime
adjust = traverse <<< PDT.adjust

diff :: LocalPreciseDateTime -> LocalPreciseDateTime -> PreciseDuration
diff (LocalValue (Locale _ m1) pdt1) (LocalValue (Locale _ m2) pdt2) =
  let offsetDiff = PD.toDecimalLossy (PD.toNanoseconds (PD.minutes (Decimal.fromNumber (unwrap (m1 - m2)))))
      dtDiff = PD.toDecimalLossy (PD.toNanoseconds (PDT.diff pdt1 pdt2))
  in PD.unsafeNanoseconds (offsetDiff + dtDiff)

fromRFC3339String :: RFC3339String -> Maybe LocalPreciseDateTime
fromRFC3339String = do
  loc <- RFC3339String.toLocale
  pdt <- PDT.fromRFC3339String <<< RFC3339String.setLocaleToZ
  pure $ LocalValue <$> loc <*> pdt

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
