module Worker where

import Client.Dispatch (AppEffects, Dispatch)
import Client.Router (router)
import Client.State (State, Route(RegisterPage), initialState, update, Action(Nav))
import Client.UI (hashChangedChannel, wEventsChannel, patchesChannel)
import Control.Apply ((*>))
import Control.Monad.Aff (later', runAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.Eff.Ref (writeRef, readRef, newRef)
import Data.Argonaut.Decode (decodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Either (either)
import Data.StrMap (empty)
import Prelude (show, void, Unit, unit, pure, ($), (>>=), (<<<), bind)
import VirtualDOM (serializePatch, diff, props, div)
import VirtualDOM.Worker (mkWorkerFunctionsForWEvents)
import WebWorker.Channel (onmessageC, registerChannel, postMessageC)

main :: Eff AppEffects Unit
main = do
  {functionSerializer, handler} <- mkWorkerFunctionsForWEvents
  let initial = div (props []) []
  stateRef <- newRef initialState
  treeRef <- newRef initial
  let dispatch act = do
        s <- readRef stateRef
        let newS = update act s
        -- effects can make new actions, so we defer it to avoid
        -- one state update overwriting the other
        defer 1 (effects dispatch newS act)
        oldTree <- readRef treeRef
        let newTree = router dispatch newS
        let patches = diff oldTree newTree
        let serializedPatches = serializePatch functionSerializer patches
        writeRef stateRef newS
        writeRef treeRef newTree
        postMessageC patchesChannel serializedPatches
  let reg1 = registerChannel empty wEventsChannel handler
  let chs = registerChannel reg1 hashChangedChannel
        (\str -> either (\_ -> dispatch (Nav RegisterPage)) (dispatch <<< Nav) (jsonParser str >>= decodeJson))
  onmessageC chs

defer :: forall a e. Int -> Eff (console :: CONSOLE | e) a -> Eff (console :: CONSOLE | e) Unit
defer i eff = void $ runAff (log <<< show)
                            (\_ -> pure unit)
                            (later' i (liftEff eff))

effects :: forall e. Dispatch -> State -> Action -> Eff (e) Unit
effects _ _ _ = pure unit
