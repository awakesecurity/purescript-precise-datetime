module Data.RFC3339String where

import Prelude

import Control.Monad.Eff (Eff, runPure)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Data.DateTime (DateTime)
import Data.DateTime.Locale (Locale(..))
import Data.Either (hush)
import Data.Foldable (foldr)
import Data.Formatter.DateTime (format)
import Data.Int (fromString, toNumber)
import Data.JSDate (JSDate, LOCALE)
import Data.JSDate as JSDate
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype, unwrap)
import Data.RFC3339String.Format (iso8601Format)
import Data.String as String
import Data.String.Regex (match, regex, replace) as RE
import Data.String.Regex.Flags (noFlags) as RE
import Data.String.Regex.Unsafe (unsafeRegex) as RE
import Data.Time.Duration (Hours(..), Minutes(..), convertDuration)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..), snd)
import Partial.Unsafe (unsafePartial)

newtype RFC3339String = RFC3339String String

derive instance newtypeRFC3339String :: Newtype RFC3339String _
derive newtype instance eqRFC3339String :: Eq RFC3339String
derive newtype instance ordRFC3339String :: Ord RFC3339String

instance showRFC3339String :: Show RFC3339String where
  show (RFC3339String s) = "(RFC3339String " <> show s <> ")"

-- | Remove trailing zeros from the subsecond component.
trim :: RFC3339String -> RFC3339String
trim (RFC3339String s) =
  let
    withoutZulu = dropWhileEnd (eq 'Z') s
    withoutTrailingZeros = dropWhileEnd (eq '0') withoutZulu
    withoutTrailingDot = dropWhileEnd (eq '.') withoutTrailingZeros
  in
    if withoutTrailingZeros == withoutTrailingDot
    then RFC3339String $ withoutTrailingDot <> "Z"
         -- always have a subseconds component
    else RFC3339String $ withoutTrailingDot <> ".0Z"

-- | Use our own formatter since we'd otherwise need to convert from `DateTime`
-- | to `JSDate` first, and `Data.JSDate.toISOString` can throw exceptions.
fromDateTime :: DateTime -> RFC3339String
fromDateTime = trim <<< RFC3339String <<< format iso8601Format

-- | Reads the locale, returning GMT (+0000) if not present.
toLocale :: RFC3339String -> Locale
toLocale (RFC3339String s) = Locale Nothing $ fromMaybe zero $ unsafePartial $ do
  re <- hush $ RE.regex "([-|\\+])(\\d\\d):?(\\d\\d)$" RE.noFlags
  [_, sign, hrs, mins] <- sequence =<< RE.match re s
  let readNum = map toNumber <<< fromString
  hrs' <- readNum hrs
  mins' <- readNum mins
  guard $ 0 <= hrs' && hrs' <= 24
  guard $ if hrs' == 24 then mins' == 0 else 0 <= mins' && mins' <= 59
  let offset = convertDuration (Hours hrs') + Minutes mins'
  pure $ (if sign == "-" then negate else id) offset

-- | Normalise the locale to GMT.
normLocale :: RFC3339String -> RFC3339String
normLocale (RFC3339String s) =
  let re = RE.unsafeRegex "([-|\\+])(\\d\\d):?(\\d\\d)$" RE.noFlags
  in RFC3339String (RE.replace re "Z" s)

toDateTime :: RFC3339String -> Maybe DateTime
toDateTime = JSDate.toDateTime <<< unsafeParse <<< unwrap
  where
  coerceJSDate :: Eff (locale :: LOCALE) JSDate -> Eff () JSDate
  coerceJSDate = unsafeCoerceEff

  -- | Parse a `String` that is known to specify a time zone.
  -- |
  -- | See https://github.com/purescript-contrib/purescript-js-date/issues/15
  -- | for why this is "unsafe".
  unsafeParse :: String -> JSDate
  unsafeParse = runPure <<< coerceJSDate <<< JSDate.parse

-- | Returns the prefix remaining after dropping characters that satisfy the
-- | predicate from the end of the string.
dropWhileEnd :: (Char -> Boolean) -> String -> String
dropWhileEnd p s = snd $ foldr check (Tuple false "") (String.toCharArray s)
  where
    check c state@(Tuple false _) = if p c then state else Tuple true (String.singleton c)
    check c state@(Tuple true string) = Tuple true (String.singleton c <> string)
