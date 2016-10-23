module Client.Router where

import Client.Action (AppEffects, Action)
import Client.Page.Start (start)
import Client.State (Route, State)
import Control.Monad.Eff (Eff)
import Prelude ((>=>), (>>=), (>>>), Unit, unit, pure)
import VirtualDOM (VTree)

router :: State -> VTree
router s = start s

loadPage :: (Action -> Eff AppEffects Unit) -> State -> Route -> Eff AppEffects Unit
loadPage s _ _ = pure unit
