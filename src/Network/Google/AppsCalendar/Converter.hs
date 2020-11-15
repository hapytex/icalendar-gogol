{-# LANGUAGE OverloadedStrings #-}

module Network.Google.AppsCalendar.Converter (
    convert
  ) where

import Control.Lens.Setter(ASetter, set)

import Data.Text(Text, pack)
import Data.Text.Lazy(toStrict)
import qualified Data.Text.Lazy as TL

import Network.Google.AppsCalendar.Types(
    Event, event
  , eCreated, eDescription, eHTMLLink, eLocation, eStatus, eSummary, eTransparency, eUpdated
  )
import Network.URI(URI, uriToString)

import Text.ICalendar.Types(
    VEvent(veCreated, veDescription, veLastMod, veLocation, veStatus, veSummary, veTransp, veUrl)
  , Created(createdValue)
  , Description(descriptionValue)
  , EventStatus(CancelledEvent, ConfirmedEvent, TentativeEvent)
  , LastModified(lastModifiedValue)
  , Location(locationValue)
  , Summary(summaryValue)
  , TimeTransparency(Opaque, Transparent)
  , URL(urlValue)
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

convert :: VEvent -> Event
convert ev = foldr ($ ev) event [setDescription, setSummary, setLocation, setUpdated, setHTMLLink, setStatus, setCreated, setTransparency]
