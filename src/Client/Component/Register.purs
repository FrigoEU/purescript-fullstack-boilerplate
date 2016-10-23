module Client.Component.Register where

import Client.Action (Action(Register_Password2Changed, Register_PasswordChanged, Register_EmailChanged, Register_Go))
import Client.SEvents (change, on, submit, on')
import Client.State (Async(Done))
import Data.Either (Either(Left))
import Data.Maybe (Maybe(Just, Nothing))
import Data.Monoid (mempty)
import Model.User (RegisterError(InvalidEmail, DifferentPasswords, PasswordTooShort, EmailInUse), SessionDetails)
import Prelude ((==))
import VirtualDOM (vtext, VTree)
import VirtualDOM.HTML (span, cl, typeP, button, form, value, name, input)

register :: { email :: String
         , password :: String
         , password2 :: String
         }
         -> Async (Either RegisterError SessionDetails)
         -> VTree
register s as =
  form [on' submit Register_Go]
    [ withErr em (emailErrorMess as)
    , withErr pw (pwErrorMess as)
    , withErr pw2 (pw2ErrorMess as)
    , button [typeP "submit"] [vtext "Go!"]
    ]
  where em = input [ on change Register_EmailChanged {val: mempty}
                   , typeP "email"
                   , name "email"
                   , value s.email] []
        pw = input [ on change Register_PasswordChanged {val: mempty}
                   , typeP "password"
                   , name "password"
                   , value s.password] []
        pw2 = input [ on change Register_Password2Changed {val: mempty}
                    , typeP "password"
                    , name "password"
                    , value s.password2] []


emailErrorMess :: forall a. Async (Either RegisterError a) -> Maybe String
emailErrorMess (Done (Left EmailInUse)) = Just "Email in use"
emailErrorMess (Done (Left InvalidEmail)) = Just "Invalid Email"
emailErrorMess _ = Nothing

pwErrorMess :: forall a. Async (Either RegisterError a) -> Maybe String
pwErrorMess (Done (Left DifferentPasswords)) = Just "Passwords don't match"
pwErrorMess (Done (Left PasswordTooShort)) = Just "Password is too short"
pwErrorMess _ = Nothing

pw2ErrorMess :: forall a. Async (Either RegisterError a) -> Maybe String
pw2ErrorMess (Done (Left DifferentPasswords)) = Just "Passwords don't match"
pw2ErrorMess _ = Nothing

withErr :: VTree -> Maybe String -> VTree
withErr input Nothing = input
withErr input (Just vali) =
  span [cl "has-error"] [ input , span [] [vtext vali]]
