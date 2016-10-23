module Model.User where

import Data.Argonaut.Decode (gDecodeJson, class DecodeJson)
import Data.Argonaut.Encode (gEncodeJson, class EncodeJson)
import Data.Generic (gShow, class Generic)
import Data.Show (class Show)
import Data.String.Regex (Regex)
import Model.UUID (UUID)
import Prelude ((<>))

newtype SessionId = SessionId UUID

newtype UserId = UserId UUID

newtype SessionDetails = SessionDetails { sessionId :: SessionId
                                        , email :: String
                                        , userId :: UserId
                                        }

newtype UserPass = UserPass {email :: String, password :: String}

data RegisterError = EmailInUse
                   | DifferentPasswords
                   | PasswordTooShort
                   | InvalidEmail

foreign import emailRE :: Regex

------ INSTANCES ---------------

-- TODO change to newtype deriving when 0.10.0 is more stable (if possible?)
derive instance genericUserId :: Generic UserId
instance encodeJsonUserId :: EncodeJson UserId where encodeJson = gEncodeJson
instance decodeJsonUserId :: DecodeJson UserId where decodeJson = gDecodeJson

derive instance genericSessionId :: Generic SessionId
instance encodeJsonSessionId :: EncodeJson SessionId where encodeJson = gEncodeJson
instance decodeJsonSessionId :: DecodeJson SessionId where decodeJson = gDecodeJson

derive instance genericSessionDetails :: Generic SessionDetails
instance encodeJsonSessionDetails :: EncodeJson SessionDetails where encodeJson = gEncodeJson
instance decodeJsonSessionDetails :: DecodeJson SessionDetails where decodeJson = gDecodeJson

derive instance genericUserPass :: Generic UserPass
instance encodeJsonUserPass :: EncodeJson UserPass where encodeJson = gEncodeJson
instance decodeJsonUserPass :: DecodeJson UserPass where decodeJson = gDecodeJson
instance showUserPass :: Show UserPass where
  show (UserPass up) = "UserPass {email: " <> up.email <> ", password: ******}"

derive instance genericRegisterError :: Generic RegisterError
instance encodeJsonRegisterError :: EncodeJson RegisterError
  where encodeJson = gEncodeJson
instance decodeJsonRegisterError :: DecodeJson RegisterError
  where decodeJson = gDecodeJson
