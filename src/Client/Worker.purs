module Client.Worker where

import Client.Action (actionsChannel, AppEffects, update, Action, effects)
import Client.Channels (patchesChannel)
import Client.Router (router)
import Client.State (initialState)
import Control.Apply ((*>))
import Control.Monad.Aff (later', runAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Ref (writeRef, readRef, newRef)
import Data.Function.Uncurried (mkFn2)
import Data.StrMap (empty)
import Prelude ((<>), show, void, Unit, unit, pure, ($), (>>=), (<<<), bind)
import VirtualDOM (FunctionSerializer, serializePatch, diff)
import VirtualDOM.HTML (div)
import WebWorker.Channel (registerChannel, onmessageC, postMessageC)

type Dispatch = Action -> Eff AppEffects Unit

main :: Eff AppEffects Unit
main = do
  -- Initial render to sync DOM states with UI thread
  let initial = div [] []
  stateRef <- newRef initialState
  treeRef <- newRef initial

  -- Define dispatch function
  let dispatch act = do
        s <- readRef stateRef
        -- Run "interpreters" update and effects
        --   (deferred since effects can trigger new dispatch)
        let newS = update act s
        defer 1 (effects dispatch act newS)

        -- Render with new State
        oldTree <- readRef treeRef
        let newTree = router newS

        -- Diff, serialize and send patches
        let patches = diff oldTree newTree
        let serializedPatches = serializePatch dummyFunctionSerializer patches
        postMessageC patchesChannel serializedPatches

        -- Save refs
        writeRef stateRef newS
        writeRef treeRef newTree
  -- Register handlers for incoming messages from UI thread
  onmessageC (registerChannel empty actionsChannel dispatch)

dummyFunctionSerializer :: FunctionSerializer
dummyFunctionSerializer = mkFn2 (\_ _ -> {prop: "", id: ""})

defer :: forall a e. Int -> Eff (console :: CONSOLE | e) a -> Eff (console :: CONSOLE | e) Unit
defer i eff = void $ runAff (log <<< show)
                            (\_ -> pure unit)
                            (later' i (liftEff eff))
