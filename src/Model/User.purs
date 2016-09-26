module Model.User where

newtype User = User { email :: String }

newtype UserId = UserId Int

newtype SessionDetails = SessionDetails { session :: String
                                        , email :: String
                                        , userId :: UserId
                                        }
