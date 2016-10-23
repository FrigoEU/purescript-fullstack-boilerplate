module Client.Page.Start where

import Client.Component.Login (login)
import Client.Component.Register (register)
import Client.State (State, Async(..))
import Data.Maybe (Maybe(Just, Nothing))
import Model.UUID (UUID(UUID))
import Model.User (SessionId(SessionId), SessionDetails(SessionDetails))
import Prelude ((<>))
import VirtualDOM (VTree, vtext)
import VirtualDOM.HTML (div)

start :: State -> VTree
start s = div [] [ login s.forms.login s.async.login
                 , vtext (showMain s.async.login)
                 , register s.forms.register s.async.register
                 , vtext (showSess s.global.session)
                 ]

showMain :: Async (Maybe SessionDetails) -> String
showMain Initial = "initial"
showMain Busy = "busy"
showMain (Errored e) = "Failed: " <> e
showMain (Done Nothing) = "Wrong username/password"
showMain (Done (Just (SessionDetails {email}))) = "logged in: " <> email

showSess :: Maybe SessionDetails -> String
showSess Nothing = "No session"
showSess (Just (SessionDetails {sessionId: (SessionId (UUID s))})) = "Logged in: " <> s
