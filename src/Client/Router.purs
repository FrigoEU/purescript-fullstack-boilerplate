module Client.Router where

import Client.Dispatch (AppEffects, Dispatch)
import Client.State (Route, State)
import Control.Monad.Eff (Eff)
import Prelude (Unit, unit, pure)
import VirtualDOM (props, div, VTree)

router :: Dispatch -> State -> VTree
router d s = div (props []) []

loadPage :: Dispatch -> State -> Route -> Eff AppEffects Unit
loadPage s _ _ = pure unit
