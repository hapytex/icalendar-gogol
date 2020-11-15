module Network.Google.AppsCalendar.Converter (
    convert
  ) where

import Control.Lens.Setter(ASetter, set)

import Data.Text(Text)
import Data.Text.Lazy(toStrict)
import qualified Data.Text.Lazy as TL

import Network.Google.AppsCalendar.Types(
    CalendarListEntry, calendarListEntry
  , cleDescription, cleSummary, cleLocation
  )

import Text.ICalendar.Types(
    VEvent(veDescription, veSummary, veLocation)
  , Description(descriptionValue)
  , Location(locationValue)
  , Summary(summaryValue)
  )

_setFunctor :: Functor f => ASetter s t u (f c) -> (a -> f b) -> (b -> c) -> a -> s -> t
_setFunctor s f g ev = set s (g <$> f ev)

_setFunctorStrict :: Functor f => ASetter s t u (f Text) -> (a -> f b) -> (b -> TL.Text) -> a -> s -> t
_setFunctorStrict s f g = _setFunctor s f (toStrict . g)

setSummary :: VEvent -> CalendarListEntry -> CalendarListEntry
setSummary = _setFunctorStrict cleSummary veSummary summaryValue

setDescription :: VEvent -> CalendarListEntry -> CalendarListEntry
setDescription = _setFunctorStrict cleDescription veDescription descriptionValue

setLocation :: VEvent -> CalendarListEntry -> CalendarListEntry
setLocation = _setFunctorStrict cleLocation veLocation locationValue

convert :: VEvent -> CalendarListEntry
convert ev = foldr ($ ev) calendarListEntry [setDescription, setSummary, setLocation]
