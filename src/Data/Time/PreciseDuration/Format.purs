module Data.Time.PreciseDuration.Format where

import Prelude

import Data.Array as Array
import Data.Decimal (Decimal, modulo)
import Data.Maybe (Maybe(..))
import Data.Monoid (guard)
import Data.Time.PreciseDuration (PreciseDuration)
import Data.Time.PreciseDuration as PD

-- | When `intermediateZeroes` is set to `true`, zero-value components that
-- | appear in the middle of the duration will be shown.
-- |
-- | For example:
-- |
-- | ```purescript
-- | -- "1h 0m 1s"
-- | formatPreciseDuration { intermediateZeroes: true }
-- |
-- | -- "1h 1s"
-- | formatPreciseDuration { intermediateZeroes: false }
-- | ```
type Format =
  { intermediateZeroes :: Boolean
  }

formatPreciseDuration :: PreciseDuration -> String
formatPreciseDuration = formatPreciseDuration' { intermediateZeroes: true }

formatPreciseDuration' :: Format -> PreciseDuration -> String
formatPreciseDuration' format = PD.toNanoseconds >>> PD.toDecimalLossy >>> go

  where

  go :: Decimal -> String
  go ns' = sign <> (Array.intercalate " " $ Array.catMaybes components)

    where

    negative = ns' < zero
    ns = if negative then -ns' else ns'
    sign = if negative then "-" else ""

    rDays = ns `modulo` PD.week
    weeks = ns - rDays

    rHours = rDays `modulo` PD.day
    days = rDays - rHours

    rMins = rHours `modulo` PD.hour
    hours = rHours - rMins

    rSecs = rMins `modulo` PD.minute
    mins = rMins - rSecs

    secs = rSecs

    gteWeek = ns >= PD.week
    gteDay = ns >= PD.day
    gteHour = ns >= PD.hour
    gteMin = ns >= PD.minute

    ltWeek = ns < PD.week
    ltDay = ns < PD.day
    ltHour = ns < PD.hour
    ltMin = ns < PD.minute

    zDaysXHours = days == zero && hours > zero
    zHoursXMins = hours == zero && mins > zero
    zMinsXSecs = mins == zero && secs > zero

    components :: Array (Maybe String)
    components =
      [ guard
          gteWeek
          (Just $ nsToString PD.toWeeks weeks)
      , guard
          (gteDay && ltWeek || gteWeek && (days > zero || format.intermediateZeroes && (zDaysXHours || zHoursXMins || zMinsXSecs)))
          (Just $ nsToString PD.toDays days)
      , guard
          (gteHour && ltDay || gteDay && (hours > zero || format.intermediateZeroes && (zHoursXMins || zMinsXSecs)))
          (Just $ nsToString PD.toHours hours)
      , guard
          (gteMin && ltHour || gteHour && (mins > zero || format.intermediateZeroes && zMinsXSecs))
          (Just $ nsToString PD.toMinutes mins)
      , guard
          (ltMin || gteMin && rSecs > zero)
          (Just $ nsToString PD.toSeconds secs)
      ]

  nsToString :: (PreciseDuration -> PreciseDuration) -> Decimal -> String
  nsToString fn = PD.toString <<< fn <<< PD.unsafeNanoseconds
