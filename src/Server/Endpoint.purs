module Server.Endpoint where

import Data.Either (Either)
import Data.Endpoint (Endpoint(Endpoint))
import Data.HTTP.Method (Method(POST))
import Data.Maybe (Maybe)
import Model.User (RegisterError, UserPass, SessionDetails)
import Prelude (Unit)

registerUser :: Endpoint Unit UserPass (Either RegisterError SessionDetails)
registerUser = Endpoint {method: POST, url: "/register"}

loginUser :: Endpoint Unit UserPass (Maybe SessionDetails)
loginUser = Endpoint {method: POST, url: "/login"}
