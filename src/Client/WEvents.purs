module Client.WEvents where

import Data.Argonaut.Decode (decodeJson)
import Data.Either (Either(Right))
import Data.Foreign.Class (readProp)
import Data.StrMap (empty)
import Prelude (($), (>>>), pure, unit, Unit, (>>=), (#))
import VirtualDOM.Worker (registerWEventHandler, WEvent(WEvent), WEventHandlers)

click :: WEvent Unit
click = WEvent {event: "click", tag: "click", decodeJson: decodeJson}

inputVal :: WEvent String
inputVal = WEvent {event: "keyup", tag: "inputVal", decodeJson: decodeJson}

allWEvents :: WEventHandlers
allWEvents = empty # regClick # regInputVal
  where
    regClick m = registerWEventHandler m click (\_ -> pure $ Right unit)
    regInputVal m = registerWEventHandler m inputVal
      (\e -> pure (readProp "target" e >>= readProp "value"))
