{-# LANGUAGE CPP, OverloadedStrings, TupleSections #-}

module Network.Google.AppsCalendar.Converter.ICalFormat (
    IsValue(printValue)
  , ContentPrinter, EncodingFunctions(..), prop
  , IsProperty(valueToText)
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
  , RWS, runRWS
  )

import Data.CaseInsensitive (original)
import Data.Char(ord)
import Data.List(intersperse)
import Data.Set (Set)
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
  , DateTime(FloatingDateTime, UTCDateTime, ZonedDateTime, dateTimeUTC, dateTimeFloating)
  , OtherParam(OtherParam)
  , OtherParams(OtherParams)
  , Recur(recurByDay, recurByHour, recurByMinute, recurByMonth, recurByMonthDay, recurBySecond, recurBySetPos, recurByWeekNo, recurByYearDay, recurFreq, recurInterval, recurUntilCount, recurWkSt)
  , RRule(RRule)
  , Weekday(Friday, Monday, Saturday, Sunday, Thursday, Tuesday, Wednesday)
  )

#if MIN_VERSION_time(1,5,0)
import Data.Time (defaultTimeLocale)
#else
import System.Locale (defaultTimeLocale)
#endif

data EncodingFunctions = EncodingFunctions
    { efChar2Bu  :: Char -> Builder
    , efChar2Len :: Char -> Int -- ^ How many octets the character is encoded.
    }

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

paramVals :: [(Quoting, Text)] -> ContentPrinter ()
paramVals = printN paramVal

printN :: (a -> ContentPrinter ()) -> [a] -> ContentPrinter ()
printN m = sequence_ . intersperse (putc ',') . map m

printNWeekday :: Either (Int, Weekday) Weekday -> ContentPrinter ()
printNWeekday (Left (n, w)) = printShow n >> printValue w
printNWeekday (Right x) = printValue x

printShow :: Show a => a -> ContentPrinter ()
printShow = out . pack . show

printShowUpper :: Show a => a -> ContentPrinter ()
printShowUpper = out . toUpper . pack . show

printShowN :: Show a => [a] -> ContentPrinter ()
printShowN = printN printShow

paramVal :: (Quoting, Text) -> ContentPrinter ()
paramVal (NoQuotes, t) = out t
paramVal (_, t) = putc '"' >> out t >> putc '"'

class IsValue a where
    printValue :: a -> ContentPrinter ()

class IsProperty a where
    printProperty :: a -> ContentPrinter ()
    valueToText :: a -> Text
    valueToText x = (\(_, _, y) -> toStrict (toLazyText y)) $ runRWS (printProperty x) (EncodingFunctions singleton utf8Len) 0

class ToParam a where
    toParam :: a -> [(Text, [(Quoting, Text)])]

recurPart :: (b -> Bool) -> (b -> ContentPrinter ()) -> Text -> (a -> b) -> a -> ContentPrinter ()
recurPart ch h t f x = unless (ch fx) (out t >> h fx)
    where fx = f x

recurPartNonEmpty :: ([b] -> ContentPrinter ()) -> Text -> (a -> [b]) -> a -> ContentPrinter ()
recurPartNonEmpty = recurPart null

recurShowNonEmpty :: Show b => Text -> (a -> [b]) -> a -> ContentPrinter ()
recurShowNonEmpty = recurPartNonEmpty printShowN

formattingTime :: FormatTime t => String -> t -> String
formattingTime = formatTime defaultTimeLocale

printUTCTime :: UTCTime -> ContentPrinter ()
printUTCTime = out . pack . formattingTime "%C%y%m%dT%H%M%SZ"

instance IsValue Date where
    printValue = out . pack . formattingTime "%C%y%m%d" . dateValue

instance IsValue DateTime where
    printValue dt@FloatingDateTime {} = out . pack . formattingTime "%C%y%m%dT%H%M%S" . dateTimeFloating $ dt
    printValue dt@UTCDateTime {} = printUTCTime . dateTimeUTC $ dt
    printValue dt@ZonedDateTime {} = out . pack . formattingTime "%C%y%m%dT%H%M%S" . dateTimeFloating $ dt

instance (IsValue a, IsValue b) => IsValue (Either a b) where
    printValue = either printValue printValue

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

instance IsProperty a => IsProperty (Set a) where
    printProperty = mapM_ printProperty

instance IsProperty a => IsProperty (Maybe a) where
    printProperty = maybe (pure ()) printProperty

instance (IsProperty a, IsProperty b) => IsProperty (Either a b) where
    printProperty = either printProperty printProperty

instance IsProperty RRule where
    printProperty (RRule rRuleValue rRuleOther) = ln (prop "RRULE" rRuleOther >> printValue rRuleValue)

instance ToParam OtherParams where
    toParam (OtherParams l) = fromOP <$> S.toList l
      where fromOP (OtherParam x y) = (toStrict (original x), (Optional,) . toStrict <$> y)

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
