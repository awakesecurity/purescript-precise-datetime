module Data.Time.PreciseDuration.Format where

import Prelude

import Data.Array as Array
import Data.Decimal (Decimal, modulo)
import Data.Maybe (Maybe(..))
import Data.Monoid (guard)
import Data.Time.PreciseDuration (PreciseDuration)
import Data.Time.PreciseDuration as PD

formatPreciseDuration :: PreciseDuration -> String
formatPreciseDuration = PD.toNanoseconds >>> PD.toDecimalLossy >>> go

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

    components :: Array (Maybe String)
    components =
      [ guard
          (ns >= PD.week)
          (Just $ nsToString PD.toWeeks weeks)
      , guard
          (ns >= PD.day && ns < PD.week || ns >= PD.week && rDays /= zero)
          (Just $ nsToString PD.toDays days)
      , guard
          (ns >= PD.hour && ns < PD.day || ns >= PD.day && rHours /= zero)
          (Just $ nsToString PD.toHours hours)
      , guard
          (ns >= PD.minute && ns < PD.hour || ns >= PD.hour && rMins /= zero)
          (Just $ nsToString PD.toMinutes mins)
      , guard
          (ns < PD.minute || ns >= PD.minute && rSecs /= zero)
          (Just $ nsToString PD.toSeconds secs)
      ]

  nsToString :: (PreciseDuration -> PreciseDuration) -> Decimal -> String
  nsToString fn = PD.toString <<< fn <<< PD.unsafeNanoseconds
