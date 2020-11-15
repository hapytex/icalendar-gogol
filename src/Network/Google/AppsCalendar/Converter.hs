module Network.Google.AppsCalendar.Converter (
    convert
  ) where

import Control.Lens.Setter(ASetter, set)

import Data.Text(Text, pack)
import Data.Text.Lazy(toStrict)
import qualified Data.Text.Lazy as TL

import Network.Google.AppsCalendar.Types(
    Event, event
  , eDescription, eHTMLLink, eLocation, eSummary, eUpdated
  )
import Network.URI(URI, uriToString)

import Text.ICalendar.Types(
    VEvent(veDescription, veSummary, veLastMod, veLocation, veUrl)
  , Description(descriptionValue)
  , LastModified(lastModifiedValue)
  , Location(locationValue)
  , Summary(summaryValue)
  , URL(urlValue)
  )

_showURL :: URI -> ShowS
_showURL = uriToString id

_setFunctor :: Functor f => ASetter s t u (f c) -> (a -> f b) -> (b -> c) -> a -> s -> t
_setFunctor s f g ev = set s (g <$> f ev)

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
setHTMLLink = _setFunctor eHTMLLink veUrl (pack . flip _showURL "" . urlValue)

convert :: VEvent -> Event
convert ev = foldr ($ ev) event [setDescription, setSummary, setLocation, setUpdated, setHTMLLink]
