module Client.Action where

import Client.State
import Client.Channels (setHashChannel)
import Control.Monad.Aff (Aff, runAff)
import Control.Monad.Aff.Endpoint (execEndpoint)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (message)
import Control.Monad.Eff.Ref (REF)
import Data.Argonaut.Decode (decodeJson, gDecodeJson, class DecodeJson)
import Data.Argonaut.Encode (encodeJson, gEncodeJson, class EncodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Argonaut.Printer (printJson)
import Data.Either (Either(Right, Left))
import Data.Generic (class Generic)
import Data.Lens (set, (.~))
import Data.Maybe (Maybe(Just))
import Data.String (length)
import Data.String.Regex (test)
import Model.User (emailRE, RegisterError(InvalidEmail, PasswordTooShort, DifferentPasswords), SessionDetails, UserPass(UserPass))
import Network.HTTP.Affjax (AJAX)
import Prelude (Unit, void, unit, pure, not, id, (>>>), (>=>), (<<<), ($), (<=), (/=))
import Server.Endpoint (registerUser, loginUser)
import WebWorker (IsWW)
import WebWorker.Channel (postMessageC, Channel(Channel))

type AppEffects = (ref :: REF, isww :: IsWW, console :: CONSOLE, ajax :: AJAX)
data Action = Nav Route
            | Init String (Maybe SessionDetails)
            | HashChanged String
            | Login_EmailChanged {val :: String}
            | Login_PasswordChanged {val :: String}
            | Login_Go
            | Async_Login (Async (Maybe SessionDetails))
            | Register_EmailChanged {val :: String}
            | Register_PasswordChanged {val :: String}
            | Register_Password2Changed {val :: String}
            | Register_Go
            | Async_Register (Async (Either RegisterError SessionDetails))

update :: Action -> State -> State
update a s = s { global = updateGlobal a s.global
               , async = updateAsync a s.async
               , forms = updateForms a s.forms
               }

updateGlobal :: Action -> GlobalState -> GlobalState
updateGlobal (Nav r) s = set _route r s
updateGlobal (HashChanged h) s = set _route (fromEither s.route (parseRoute h)) s
updateGlobal (Async_Login (Done (Just sess))) s = set _session (Just sess) s
updateGlobal (Async_Register (Done (Right sess))) s = set _session (Just sess) s
updateGlobal (Init h ms) s = s { session = ms
                               , route = fromEither s.route (parseRoute h)}
updateGlobal _ s = s

updateAsync :: Action -> AsyncState -> AsyncState
updateAsync (Async_Login a) = _login .~ a
updateAsync (Async_Register a) =  _register .~ a
updateAsync _ = id

updateForms :: Action -> FormsState -> FormsState
updateForms (Login_EmailChanged {val}) = (_login <<< _email) .~ val
updateForms (Login_PasswordChanged {val}) = (_login <<< _password) .~ val
updateForms (Register_EmailChanged {val}) = (_register <<< _email) .~ val
updateForms (Register_PasswordChanged {val}) = (_register <<< _password) .~ val
updateForms (Register_Password2Changed {val}) = (_register <<< _password2) .~ val
updateForms _ = id

effects :: (Action -> Eff AppEffects Unit) -> Action -> State -> Eff AppEffects Unit
effects d (Nav r) _ = postMessageC setHashChannel (encodeRoute r)
effects d (Async_Register (Done (Right s))) _ = saveSession s
effects d (Async_Login (Done (Just s))) _ = saveSession s
effects d Register_Go s | s.forms.register.password /= s.forms.register.password2 =
    d (Async_Register (Done $ Left DifferentPasswords))
effects d Register_Go s | not (test emailRE s.forms.register.email) =
    d (Async_Register (Done $ Left InvalidEmail))
effects d Register_Go s | length s.forms.register.password <= 4 =
    d (Async_Register (Done $ Left PasswordTooShort))
effects d Register_Go {forms: {register: {email, password}}} =
  dispatchAff d Async_Register
                (execEndpoint registerUser unit (UserPass {email, password}))
effects d Login_Go {forms: {login: {email, password}}} =
  dispatchAff d Async_Login (execEndpoint loginUser unit (UserPass {email, password}))
effects _ _ _ = pure unit

------------------------

dispatchAff :: forall eff a act.
              (act -> Eff eff Unit)
              -> (Async a -> act)
              -> Aff eff a
              -> Eff eff Unit
dispatchAff d async aff =
    void $ runAff (d <<< async <<< Errored <<< message)
                  (d <<< async <<< Done)
                  aff

parseRoute :: String -> Either String Route
parseRoute = jsonParser >=> decodeJson

encodeRoute :: Route -> String
encodeRoute = encodeJson >>> printJson

actionsChannel :: Channel Action
actionsChannel = Channel "actionsfam"

----------------------- Storage
-- TODO can we devise an easier way to use LS in workers?

saveSession :: forall e.
  SessionDetails -> Eff ( isww :: IsWW | e) Unit
saveSession = postMessageC sessionsChannel

sessionsChannel :: Channel SessionDetails
sessionsChannel = Channel "sessiondetailsyo"

-----------------------

derive instance genericAction :: Generic Action
instance encodeJsonAction :: EncodeJson Action where encodeJson = gEncodeJson
instance decodeJsonAction :: DecodeJson Action where decodeJson = gDecodeJson

fromEither :: forall a b. b -> Either a b -> b
fromEither b (Left _) = b
fromEither _ (Right b) = b
