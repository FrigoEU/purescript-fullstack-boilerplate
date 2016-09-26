module Server.Endpoint where

import Data.Argonaut.Decode (gDecodeJson, class DecodeJson)
import Data.Argonaut.Encode (gEncodeJson, class EncodeJson)
import Data.Endpoint (Endpoint(Endpoint))
import Data.Generic (class Generic)
import Data.HTTP.Method (Method(POST))
import Model.User (SessionDetails)
import Prelude (Unit)

registerUser :: Endpoint Unit UserPass SessionDetails
registerUser = Endpoint {method: POST, url: "/register"}

login :: Endpoint Unit UserPass SessionDetails
login = Endpoint {method: POST, url: "/login"}

newtype UserPass = UserPass {email :: String, password :: String}
derive instance genericGetMessagesQp :: Generic UserPass
instance encodeJsonGetMessagesQp :: EncodeJson UserPass where encodeJson = gEncodeJson
instance decodeJsonGetMessagesQp :: DecodeJson UserPass where decodeJson = gDecodeJson
