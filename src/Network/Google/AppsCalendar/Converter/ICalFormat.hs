{-# LANGUAGE CPP, OverloadedStrings, TupleSections #-}

{-|
Module      : Network.Google.AppsCalendar.Converter.ICalFormat
Description : A module that provides functionality to map subparts of a 'Text.ICalendar.Types.VEvent' to 'Text'.
Maintainer  : hapytexeu+gh@gmail.com
Stability   : experimental
Portability : POSIX

This module provides functions to map subparts of a 'Text.ICalendar.Types.VEvent' to 'Text'. Especially the part
of a 'Recur' object is useful here for the conversion of a 'Text.ICalendar.Types.VEvent'.

The code is basically a copy of the 'Text.ICalendar.Printer' module, which does not export functionality to print
the /subparts/ of a Text.'ICalendar.Types.VEvent' object.
-}

module Network.Google.AppsCalendar.Converter.ICalFormat (
    -- * Typeclass to map subparts.
    IsValue(printValue)
    -- * Types to make printing more convenient.
  , ContentPrinter, EncodingFunctions(efChar2Bu, efChar2Len)
    -- * Printing properties.
  , IsProperty(printProperty, propertyToText)
  ) where

import Control.Arrow ((&&&))
import Control.Monad (forM_, unless, when)
import Control.Monad.RWS (
    put
  , asks
  , tell
  , get
  , modify
--    MonadState (get, put)
--  , MonadWriter (tell), RWS, asks
--  , modify, runRWS
  , RWS, evalRWS
  )

import Data.CaseInsensitive (original)
import Data.Char(ord)
import Data.List(intersperse)
import Data.Set (Set, maxView)
import Data.Text (Text, pack, toUpper)
import qualified Data.Text as T
import Data.Text.Lazy (toStrict)
import Data.Time (FormatTime, UTCTime, formatTime)
import qualified Data.Set as S
import Data.Text.Lazy.Builder(
    Builder, fromText, singleton, toLazyText
  )

import Text.ICalendar.Types (
    Date(dateValue)
  , DateTime(FloatingDateTime, UTCDateTime, ZonedDateTime, dateTimeUTC, dateTimeFloating, dateTimeZone)
  , Duration(DurationDate, DurationTime, DurationWeek)
  , ExDate(ExDates, ExDateTimes)
  , OtherParam(OtherParam)
  , OtherParams(OtherParams)
  , Period(PeriodDates, PeriodDuration)
  , Recur(recurByDay, recurByHour, recurByMinute, recurByMonth, recurByMonthDay, recurBySecond, recurBySetPos, recurByWeekNo, recurByYearDay, recurFreq, recurInterval, recurUntilCount, recurWkSt)
  , RDate(RDateDates, RDateDateTimes, RDatePeriods, rDateDates, rDateDateTimes, rDatePeriods, rDateOther)
  , RRule(RRule)
  , Sign(Positive, Negative)
  , Weekday(Friday, Monday, Saturday, Sunday, Thursday, Tuesday, Wednesday)
  )

#if MIN_VERSION_time(1,5,0)
import Data.Time (defaultTimeLocale)
#else
import System.Locale (defaultTimeLocale)
#endif

-- | A datatype that is used as state to construct a textual representation of the objects.
data EncodingFunctions = EncodingFunctions
    { efChar2Bu  :: Char -> Builder  -- ^ A function to convert a single character into a 'Builder' used to construct singleton text objects.
    , efChar2Len :: Char -> Int -- ^ How many octets the character is encoded.
    }

-- | A type synonym of a way to print data. This makes use of the 'Builder' to generate the stream and the 'EncodingFunctions' as state.
type ContentPrinter = RWS EncodingFunctions Builder Int

data Quoting = NeedQuotes | Optional | NoQuotes deriving (Eq, Ord, Show)

utf8Len :: Char -> Int
utf8Len c
    | o < 0x80  = 1
    | o < 0x800  = 2
    | o < 0x10000 = 3
    | o < 0x200000 = 4
    | o < 0x4000000 = 5
    | otherwise = 6
    where o = ord c

tellBuild :: Text -> ContentPrinter ()
tellBuild = tell . fromText

newline :: ContentPrinter ()
newline = tellBuild "\r\n" >> put 0

foldLine :: ContentPrinter ()
foldLine = tellBuild "\r\n " >> put 1

ln :: ContentPrinter () -> ContentPrinter ()
ln = (>> newline)

param :: (Text, [(Quoting, Text)]) -> ContentPrinter ()
param (n, xs) = putc ';' >> out n >> putc '=' >> paramVals xs

paramVals :: Foldable f => f (Quoting, Text) -> ContentPrinter ()
paramVals = printN paramVal

printN :: Foldable f => (a -> ContentPrinter ()) -> f a -> ContentPrinter ()
printN m = sequence_ . intersperse (putc ',') . foldr ((:) . m) []

printNWeekday :: Either (Int, Weekday) Weekday -> ContentPrinter ()
printNWeekday (Left (n, w)) = printShow n >> printValue w
printNWeekday (Right x) = printValue x

printShow :: Show a => a -> ContentPrinter ()
printShow = out . pack . show

printShowUpper :: Show a => a -> ContentPrinter ()
printShowUpper = out . toUpper . pack . show

printShowN :: (Foldable f, Show a) => f a -> ContentPrinter ()
printShowN = printN printShow

paramVal :: (Quoting, Text) -> ContentPrinter ()
paramVal (NoQuotes, t) = out t
paramVal (_, t) = putc '"' >> out t >> putc '"'

-- | A typeclass that specifies that the given type can be printed
-- as specified by the iCal format.
class IsValue a where
    -- | Convert the given item to a 'ContentPrinter' that prints it
    -- as specified by the iCal format.
    printValue
      :: a  -- ^ The given element to print.
      -> ContentPrinter ()  -- ^ a builder that will print the specified object.

-- | A typeclass that is used to print items as properties of a /parent/ element.
class IsProperty a where
    -- | print the element as specified by the iCal format.
    printProperty
      :: a  -- ^ The given element to print.
      -> ContentPrinter ()  -- ^ A builder that will print the given property.
    -- | Convert the given element to a 'Text' object that is the iCal format of that element.
    propertyToText
      :: a  -- ^ The given element to convert to 'Text'.
      -> Text  -- ^ The textual presentation of the element as a /property/.
    propertyToText x = toStrict . toLazyText . snd $ evalRWS (printProperty x) (EncodingFunctions singleton utf8Len) 0

-- | A typeclass that provides a function to obtain the parameter of a given item.
class ToParam a where
    -- | Obtain the properties of the given element as a list of 2-tuples with the name, the value and the corresponding quotation style.
    toParam
      :: a  -- ^ The given object to obtain the properties from.
      -> [(Text, [(Quoting, Text)])]  -- ^ The properties of the object.

recurPart :: (b -> Bool) -> (b -> ContentPrinter ()) -> Text -> (a -> b) -> a -> ContentPrinter ()
recurPart ch h t f x = unless (ch fx) (out t >> h fx)
    where fx = f x

recurPartNonEmpty :: ([b] -> ContentPrinter ()) -> Text -> (a -> [b]) -> a -> ContentPrinter ()
recurPartNonEmpty = recurPart null

recurShowNonEmpty :: Show b => Text -> (a -> [b]) -> a -> ContentPrinter ()
recurShowNonEmpty = recurPartNonEmpty printShowN

formattingTime :: FormatTime t => String -> t -> ContentPrinter ()
formattingTime fmt = out . pack . formatTime defaultTimeLocale fmt

printUTCTime :: UTCTime -> ContentPrinter ()
printUTCTime = formattingTime "%C%y%m%dT%H%M%SZ"

instance IsValue Date where
    printValue = formattingTime "%C%y%m%d" . dateValue

instance IsValue DateTime where
    printValue dt@FloatingDateTime {} = (formattingTime "%C%y%m%dT%H%M%S" . dateTimeFloating) dt
    printValue dt@UTCDateTime {} = (printUTCTime . dateTimeUTC) dt
    printValue dt@ZonedDateTime {} = (formattingTime "%C%y%m%dT%H%M%S" . dateTimeFloating) dt


_printDurationHms :: Int -> Int -> Int -> ContentPrinter ()
_printDurationHms h m s = putc 'T' >> printShow h >> putc 'H' >> printShow m >> putc 'M' >> printShow s >> putc 'S'

_printDurationSign :: Sign -> ContentPrinter ()
_printDurationSign Positive = putc 'P'
_printDurationSign Negative = putc '-' >> putc 'P'

instance IsValue Duration where
    printValue (DurationDate sg d h m s) = _printDurationSign sg >> printShow d >> putc 'D' >> _printDurationHms h m s
    printValue (DurationTime sg h m s) = _printDurationSign sg >> _printDurationHms h m s
    printValue (DurationWeek sg w) = _printDurationSign sg >> printShow w >> putc 'W'

instance (IsValue a, IsValue b) => IsValue (Either a b) where
    printValue = either printValue printValue

instance IsValue Period where
    printValue (PeriodDates f t) = printValue f >> putc '/' >> printValue t
    printValue (PeriodDuration f d) = printValue f >> putc '/' >> printValue d

instance IsValue Recur where
    printValue r = do
        out "FREQ="
        printShowUpper (recurFreq r)
        forM_ (recurUntilCount r) (either ((out ";UNTIL=" >>) . printValue) ((out ";COUNT=" >>) . printShow))
        recurPart (1 ==) printShow ";INTERVAL=" recurInterval r
        recurShowNonEmpty ";BYSECOND=" recurBySecond r
        recurShowNonEmpty ";BYMINUTE=" recurByMinute r
        recurShowNonEmpty ";BYHOUR=" recurByHour r
        recurPartNonEmpty (printN printNWeekday) ";BYDAY=" recurByDay r
        recurShowNonEmpty ";BYMONTHDAY=" recurByMonthDay r
        recurShowNonEmpty ";BYYEARDAY=" recurByYearDay r
        recurShowNonEmpty ";BYWEEKNO=" recurByWeekNo r
        recurShowNonEmpty ";BYMONTH=" recurByMonth r
        recurShowNonEmpty ";BYSETPOS=" recurBySetPos r
        recurPart (Monday ==) printValue ";WKST=" recurWkSt r

instance IsValue Weekday where
    printValue = out . go
        where go Sunday = "SU"
              go Monday = "MO"
              go Tuesday = "TU"
              go Wednesday = "WE"
              go Thursday = "TH"
              go Friday = "FR"
              go Saturday = "SA"

instance IsValue RDate where
    printValue r@RDateDates {} = printN printValue (rDateDates r)
    printValue r@RDateDateTimes {} = printN printValue (rDateDateTimes r)
    printValue r@RDatePeriods {} = printN printValue (rDatePeriods r)

instance IsProperty a => IsProperty (Set a) where
    printProperty = mapM_ printProperty

instance IsProperty a => IsProperty (Maybe a) where
    printProperty = maybe (pure ()) printProperty

instance (IsProperty a, IsProperty b) => IsProperty (Either a b) where
    printProperty = either printProperty printProperty

instance IsProperty ExDate where
    printProperty exd = ln $ do
        prop "EXDATE" exd
        case exd of
             ExDates e _ -> printN printValue e
             ExDateTimes e _ -> printN printValue e

instance IsProperty RDate where
    printProperty r = ln $ prop "RDATE" r >> printValue r

instance IsProperty RRule where
    printProperty (RRule rRuleValue rRuleOther) = ln (prop "RRULE" rRuleOther >> printValue rRuleValue)

instance ToParam DateTime where
    toParam dt@ZonedDateTime {} = [("TZID", [(NoQuotes, toStrict (dateTimeZone dt))])]
    toParam _ = []

instance ToParam ExDate where
    toParam (ExDates _ o) = ("VALUE", [(NoQuotes, "DATE")]) : toParam o
    toParam (ExDateTimes s o) = toParam o <> toParam (fst <$> maxView s)

instance ToParam a => ToParam (Maybe a) where
    toParam = maybe [] toParam

instance ToParam OtherParams where
    toParam (OtherParams l) = fromOP <$> S.toList l
      where fromOP (OtherParam x y) = (toStrict (original x), (Optional,) . toStrict <$> y)

instance ToParam Period where
    toParam (PeriodDates x _) = toParam x
    toParam (PeriodDuration x _) = toParam x

instance ToParam RDate where
    toParam r@RDateDates {} = ("VALUE", [(NoQuotes, "DATE")]) : toParam (rDateOther r)
    toParam r@RDatePeriods {} = ("VALUE", [(NoQuotes, "PERIOD")]) : toParam (rDateOther r) ++ toParam (fst <$> maxView (rDatePeriods r))
    toParam r@RDateDateTimes {} = toParam (rDateDateTimes r) <> toParam (rDateOther r)

instance ToParam a => ToParam (Set a) where
    toParam s = case maxView s of
        Nothing -> []
        Just (x, _) -> toParam x

prop :: ToParam a => Text -> a -> ContentPrinter ()
prop b x = do
    put (fromIntegral $ T.length b)
    tellBuild b
    mapM_ param (toParam x)
    putc ':'

out :: Text -> ContentPrinter ()
out t = case T.uncons t of
    Just (c, r) -> putc c >> out r
    Nothing -> return ()

putc :: Char -> ContentPrinter ()
putc c = do
    x <- get
    (b, clen) <- asks (efChar2Bu &&& efChar2Len)
    let cl = clen c
    when (x + cl > 75) foldLine
    tell (b c)
    modify (cl +)
