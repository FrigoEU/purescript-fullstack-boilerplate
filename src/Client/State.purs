module Client.State where

import Data.Argonaut.Decode (class DecodeJson, gDecodeJson)
import Data.Argonaut.Encode (class EncodeJson, gEncodeJson)
import Data.Either (Either)
import Data.Generic (class Generic)
import Data.Lens (LensP, lens)
import Data.Maybe (Maybe(Nothing))
import Model.User (RegisterError, SessionDetails)

data Async a = Initial
             | Busy
             | Errored String
             | Done a

data Route = StartPage

type FormsState = { register :: { email :: String , password :: String, password2 :: String}
                  , login :: { email :: String , password :: String}
                  }
type AsyncState = { register :: Async (Either RegisterError SessionDetails)
                  , login :: Async (Maybe SessionDetails)}
type GlobalState = { session :: Maybe SessionDetails
                   , route :: Route
                   }

type State = { forms :: FormsState
             , async :: AsyncState
             , global :: GlobalState
             }

initialState :: State
initialState = { forms: initialFormsState
               , async: initialAsyncState
               , global: initialGlobalState
               }

initialFormsState :: FormsState
initialFormsState = { register: { email: "" , password: "", password2: ""}
                    , login: { email: "" , password: ""}
                    }
initialAsyncState :: AsyncState
initialAsyncState = {register: Initial, login: Initial}
-- TODO better name than "GlobalState"?
initialGlobalState :: GlobalState
initialGlobalState = { session: Nothing
                     , route: StartPage
                     }

------ LENSES --------------------
_user :: forall a r. LensP {user :: a | r} a
_user = lens _.user (_ { user = _ })

_route :: forall a r. LensP {route :: a | r} a
_route = lens _.route (_ { route = _ })

_login :: forall a r. LensP {login :: a | r} a
_login = lens _.login (_ { login = _ })

_register :: forall a r. LensP {register :: a | r} a
_register = lens _.register (_ { register = _ })

_email :: forall a r. LensP {email :: a | r} a
_email = lens _.email (_ { email = _ })

_password :: forall a r. LensP {password :: a | r} a
_password = lens _.password (_ { password = _ })

_password2 :: forall a r. LensP {password2 :: a | r} a
_password2 = lens _.password2 (_ { password2 = _ })

_state :: forall a r. LensP {state :: a | r} a
_state = lens _.state (_ { state = _ })

_session :: forall a r. LensP {session :: a | r} a
_session = lens _.session (_ { session = _ })

_global :: forall a r. LensP {global :: a | r} a
_global = lens _.global (_ { global = _ })

_async :: forall a r. LensP {async :: a | r} a
_async = lens _.async (_ { async = _ })

------ DERIVING -----------------
derive instance genericAsync :: (Generic a) => Generic (Async a)

derive instance genericRoute :: Generic Route
instance decodeJsonRoute :: DecodeJson Route where decodeJson = gDecodeJson
instance encodeJsonRoute :: EncodeJson Route where encodeJson = gEncodeJson
