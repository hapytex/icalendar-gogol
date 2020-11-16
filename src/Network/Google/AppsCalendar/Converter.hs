{-# LANGUAGE OverloadedStrings #-}

module Network.Google.AppsCalendar.Converter (
    convert
  ) where

import Control.Lens.Setter(ASetter, set)

import Data.Maybe(isNothing)
import Data.Text(Text, pack)
import Data.Text.Lazy(toStrict)
import qualified Data.Text.Lazy as TL
import Data.Time.LocalTime(LocalTime, localTimeToUTC, utc)

import Network.Google.AppsCalendar.Types(
    Event, event
  , EventDateTime, eventDateTime
  , eCreated, eDescription, eEnd, eEndTimeUnspecified, eHTMLLink, eLocation, eStatus, eStart, eSummary, eTransparency, eUpdated, eVisibility
  , edtDate, edtDateTime, edtTimeZone
  )
import Network.Google.Prelude(Day, UTCTime)
import Network.URI(URI, uriToString)

import Text.ICalendar.Types(
    VEvent(veClass, veCreated, veDescription, veDTEndDuration, veDTStart, veLastMod, veLocation, veStatus, veSummary, veTransp, veUrl)
  , Class(Class)
  , ClassValue(Confidential, Public)
  , Created(createdValue)
  , DateTime(FloatingDateTime, UTCDateTime, ZonedDateTime)
  , Description(descriptionValue)
  , DurationProp
  , DTStart(DTStartDate, DTStartDateTime)
  , DTEnd(DTEndDate, DTEndDateTime)
  , EventStatus(CancelledEvent, ConfirmedEvent, TentativeEvent)
  , LastModified(lastModifiedValue)
  , Location(locationValue)
  , Summary(summaryValue)
  , TimeTransparency(Opaque, Transparent)
  , URL(urlValue)
  , dateValue
  )

_showURI :: URI -> ShowS
_showURI = uriToString id

_textURL :: URL -> Text
_textURL = pack . flip _showURI "" . urlValue

eventStatusToText :: EventStatus -> Text
eventStatusToText CancelledEvent {} = "cancelled"
eventStatusToText ConfirmedEvent {} = "confirmed"
eventStatusToText TentativeEvent {} = "tentative"

transparencyToText :: TimeTransparency -> Text
transparencyToText Opaque {} = "opaque"
transparencyToText Transparent {} = "transparent"

mkEventDateTime :: Maybe Day -> Maybe UTCTime -> Maybe Text -> EventDateTime
mkEventDateTime md mut mtz = set edtTimeZone mtz (set edtDateTime mut (set edtDate md eventDateTime))

localTimeToUtc :: LocalTime -> UTCTime
localTimeToUtc = localTimeToUTC utc

convertDateTime :: DateTime -> EventDateTime
convertDateTime (UTCDateTime u) = mkEventDateTime Nothing (Just u) Nothing
convertDateTime (ZonedDateTime lt t) = mkEventDateTime Nothing (Just (localTimeToUtc lt)) (Just (toStrict t))
convertDateTime (FloatingDateTime lt) = mkEventDateTime Nothing (Just (localTimeToUtc lt)) Nothing

convertDTStart :: Maybe Text -> DTStart -> EventDateTime
convertDTStart tz = go
    where go (DTStartDate d _) = mkEventDateTime (Just (dateValue d)) Nothing tz
          go (DTStartDateTime d _) = convertDateTime d

convertEndDuration :: Maybe Text -> Either DTEnd DurationProp -> EventDateTime
convertEndDuration tz (Left de) = convertDTEnd tz de
convertEndDuration _ (Right _) = undefined

convertDTEnd :: Maybe Text -> DTEnd -> EventDateTime
convertDTEnd tz = go
    where go (DTEndDate d _) = mkEventDateTime (Just (dateValue d)) Nothing tz
          go (DTEndDateTime d _) = convertDateTime d

convertClass :: Class -> Text
convertClass (Class cv _) = go cv
    where go Public = "public"
          go Confidential = "confidential"
          go _ = "private"

_setSimple :: ASetter s t u b -> (a -> b) -> a -> s -> t
_setSimple s f ev = set s (f ev)

_setFunctor :: Functor f => ASetter s t u (f c) -> (a -> f b) -> (b -> c) -> a -> s -> t
_setFunctor s f g = _setSimple s (fmap g . f)

_setFunctorStrict :: Functor f => ASetter s t u (f Text) -> (a -> f b) -> (b -> TL.Text) -> a -> s -> t
_setFunctorStrict s f g = _setFunctor s f (toStrict . g)

setSummary :: VEvent -> Event -> Event
setSummary = _setFunctorStrict eSummary veSummary summaryValue

setDescription :: VEvent -> Event -> Event
setDescription = _setFunctorStrict eDescription veDescription descriptionValue

setLocation :: VEvent -> Event -> Event
setLocation = _setFunctorStrict eLocation veLocation locationValue

setUpdated :: VEvent -> Event -> Event
setUpdated = _setFunctor eUpdated veLastMod lastModifiedValue

setHTMLLink :: VEvent -> Event -> Event
setHTMLLink = _setFunctor eHTMLLink veUrl _textURL

setStatus :: VEvent -> Event -> Event
setStatus = _setFunctor eStatus veStatus eventStatusToText

setCreated :: VEvent -> Event -> Event
setCreated = _setFunctor eCreated veCreated createdValue

setTransparency :: VEvent -> Event -> Event
setTransparency = _setSimple eTransparency (transparencyToText . veTransp)

setStart :: VEvent -> Event -> Event
setStart = _setFunctor eStart veDTStart (convertDTStart Nothing)

setEnd :: VEvent -> Event -> Event
setEnd = _setFunctor eEnd veDTEndDuration (convertEndDuration Nothing)

setEndTimeUnspecified :: VEvent -> Event -> Event
setEndTimeUnspecified = _setSimple eEndTimeUnspecified (isNothing . veDTEndDuration)

setVisibility :: VEvent -> Event -> Event
setVisibility = _setSimple eVisibility (convertClass . veClass)

convert :: VEvent -> Event
convert ev = foldr ($ ev) event [
    setCreated
  , setDescription
  , setEnd
  , setEndTimeUnspecified
  , setHTMLLink
  , setLocation
  , setStart
  , setStatus
  , setSummary
  , setTransparency
  , setUpdated
  , setVisibility
  ]
