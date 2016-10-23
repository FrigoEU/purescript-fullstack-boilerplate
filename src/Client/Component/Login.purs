module Client.Component.Login where

import Client.Action (Action(Login_Go, Login_PasswordChanged, Login_EmailChanged))
import Client.SEvents (change, on, submit, on')
import Client.State (Async)
import Data.Maybe (Maybe)
import Data.Monoid (mempty)
import Model.User (SessionDetails)
import VirtualDOM (vtext, VTree)
import VirtualDOM.HTML (name, typeP, button, value, input, form)

login :: { email :: String
         , password :: String
         }
         -> Async (Maybe SessionDetails)
         -> VTree
login s _ =
  form [on' submit Login_Go]
    [ input [on change Login_EmailChanged {val: mempty}
            , typeP "email"
            , name "email"
            , value s.email] []
    , input [on change Login_PasswordChanged {val: mempty}
            , typeP "password"
            , name "password"
            , value s.password] []
    , button [typeP "submit"] [vtext "Go!"]
    ]
