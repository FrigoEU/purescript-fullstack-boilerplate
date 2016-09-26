module Client.Dispatch where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Ref (REF)
import Client.State (Action)
import Network.HTTP.Affjax (AJAX)
import Prelude (Unit)
import WebWorker (IsWW)

type AppEffects = (ref :: REF, isww :: IsWW, console :: CONSOLE, ajax :: AJAX)
type Dispatch = Action -> Eff AppEffects Unit
