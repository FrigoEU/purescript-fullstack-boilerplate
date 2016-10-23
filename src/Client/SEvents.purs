module Client.SEvents where

import Client.Action (Action)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Exception (throw, EXCEPTION)
import DOM (DOM)
import DOM.Event.Event (preventDefault)
import DOM.Event.Types (Event) as DOM
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Encode (encodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Argonaut.Printer (printJson)
import Data.Either (either, Either(Right))
import Data.Exists (Exists, runExists)
import Data.Foldable (find)
import Data.Foreign (F, readString, toForeign)
import Data.Foreign.Class (readProp)
import Data.Maybe (maybe)
import Data.Monoid (class Monoid)
import Data.String (length, drop, take)
import Prelude ((*>), show, Unit, pure, unit, (>>=), ($), (==), (#), (<>))
import VirtualDOM (prop, Prop)
import WebWorker (WebWorker, OwnsWW)
import WebWorker.Channel (postMessageToWorkerC, Channel)

on :: forall a obj.
      Monoid a =>
      Event a
      -> ({val :: a | obj} -> Action)
      -> {val :: a | obj}
      -> Prop
on (Event {event, id}) constr rest =
  prop (event)
       ("_vdom_as_json_" <> id <> printJson (encodeJson (constr rest)))

on' :: forall a. Event a -> Action -> Prop
on' (Event {event, id}) action =
  prop (event)
       ("_vdom_as_json_" <> id <> printJson (encodeJson action))

type EventS = Exists Event

-- this function is (relatively) safe because on the worker thread the compiler ensured us through the signature of on/on'
foreign import magic :: forall a. Action -> a -> Action

mkUIHandlers :: Array EventS -> WebWorker -> Channel Action -> String -> DOM.Event -> Eff (dom :: DOM, ownsww :: OwnsWW, err :: EXCEPTION) Unit
mkUIHandlers events ww chan =
  \str ->
    let found = find (runExists (\(Event {id}) -> startsWith id str)) events
     in maybe (\_ -> throw ("No event handler found for " <> str))
              (runExists (\(Event {id, handle}) ->
                  either
                    (\s forn -> throw ("Failed to parse action from " <> str <> ": " <> s))
                    (\action forn -> handle forn >>= either
                                                      (\e -> throw ("Failed to extract value from event: " <> show e))
                                                      (\b -> postMessageToWorkerC ww chan (magic action b)))
                    ((drop (length id) str # jsonParser >>= decodeJson) :: Either String Action)))
              found

startsWith :: String -> String -> Boolean
startsWith short long = take (length short) long == short

newtype Event a = Event {event :: String, id :: String, handle :: DOM.Event -> Eff (dom :: DOM, ownsww :: OwnsWW, err :: EXCEPTION) (F a)}

click :: Event Unit
click = Event {event: "onclick", id: "click1", handle: \_ -> pure (Right unit)}

change :: Event String
change = Event {event: "onchange", id: "changeVal", handle: \ev -> pure $ readProp "target" (toForeign ev) >>= readProp "value" >>= readString}

submit :: Event Unit
submit = Event {
  event: "onsubmit",
  id: "submit1",
  handle: \ev -> preventDefault ev *> pure (Right unit)
}
