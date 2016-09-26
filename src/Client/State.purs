module Client.State where

import Control.Monad.Eff.Exception (Error)
import Data.Argonaut.Decode (class DecodeJson, gDecodeJson)
import Data.Argonaut.Encode (class EncodeJson, gEncodeJson)
import Data.Generic (class Generic)
import Data.Lens (set, LensP, lens)
import Data.Maybe (Maybe(Nothing))
import Model.User (User)

data Async a = Initial
             | Busy
             | Failed Error
             | Done a

data Route = RegisterPage
           | LoginPage

type State = { user :: Maybe User
             , route :: Route
             , registerS :: { email :: String
                             , password :: String
                             , state :: Async User
                             }
             , loginS :: { email :: String
                         , password :: String
                         , state :: Async User
                         }
             }

initialState :: State
initialState = { user: Nothing
               , route: RegisterPage
               , registerS: { email: ""
                           , password: ""
                           , state: Initial
                           }
               , loginS: { email: ""
                         , password: ""
                         , state: Initial
                         }
               }

data Action = Nav Route
            | Register

update :: Action -> State -> State
update (Nav r) s = set _route r s
update (Register) s = s

------ LENSES --------------------

_user :: forall a r. LensP {user :: a | r} a
_user = lens _.user (_ { user = _ })

_route :: forall a r. LensP {route :: a | r} a
_route = lens _.route (_ { route = _ })

_loginS :: forall a r. LensP {loginS :: a | r} a
_loginS = lens _.loginS (_ { loginS = _ })

_email :: forall a r. LensP {email :: a | r} a
_email = lens _.email (_ { email = _ })

_password :: forall a r. LensP {password :: a | r} a
_password = lens _.password (_ { password = _ })

_state :: forall a r. LensP {state :: a | r} a
_state = lens _.state (_ { state = _ })

------ DERIVING -----------------
derive instance genericRoute :: Generic Route
instance decodeJsonRoute :: DecodeJson Route where decodeJson = gDecodeJson
instance encodeJsonRoute :: EncodeJson Route where encodeJson = gEncodeJson
