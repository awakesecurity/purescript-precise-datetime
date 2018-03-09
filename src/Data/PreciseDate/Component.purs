module Data.PreciseDate.Component
  ( Nanosecond(..)
  ) where

import Prelude

import Data.Enum (class BoundedEnum, class Enum, Cardinality(..), fromEnum, toEnum)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)

-- | A nanosecond component for a time value.
-- |
-- | Must be used in conjunction with a millisecond component to represent the
-- | total amount of time less than 1 second.
-- |
-- | The constructor is private as values for the type are restricted to the
-- | range 0 to 999999, inclusive. The `toEnum` function can be used to
-- | safely acquire an `Nanosecond` value from an integer. Correspondingly, a
-- | `Nanosecond` can be lowered to a plain integer with the `fromEnum`
-- | function.
newtype Nanosecond = Nanosecond Int

derive instance newtypeNanosecond :: Newtype Nanosecond _
derive newtype instance eqNanosecond :: Eq Nanosecond
derive newtype instance ordNanosecond :: Ord Nanosecond

instance boundedNanosecond :: Bounded Nanosecond where
  bottom = Nanosecond 0
  top = Nanosecond 999999

instance enumNanosecond :: Enum Nanosecond where
  succ = toEnum <<< (_ + 1) <<< fromEnum
  pred = toEnum <<< (_ - 1) <<< fromEnum

instance boundedEnumNanosecond :: BoundedEnum Nanosecond where
  cardinality = Cardinality 1000000
  toEnum n
    | n >= 0 && n <= 999999 = Just (Nanosecond n)
    | otherwise = Nothing
  fromEnum (Nanosecond n) = n

instance showNanosecond :: Show Nanosecond where
  show (Nanosecond n) = "(Nanosecond " <> show n <> ")"
