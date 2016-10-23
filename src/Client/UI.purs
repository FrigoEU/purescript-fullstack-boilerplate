module Client.UI where

import Client.Action (sessionsChannel, actionsChannel, Action(Init, HashChanged))
import Client.Channels (setHashChannel, patchesChannel)
import Client.SEvents (submit, EventS, mkUIHandlers, change, click)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import DOM (DOM)
import DOM.Event.EventTarget (eventListener, addEventListener)
import DOM.HTML (window)
import DOM.HTML.Event.EventTypes (hashchange)
import DOM.HTML.Location (hash, setHash)
import DOM.HTML.Types (windowToEventTarget)
import DOM.HTML.Window (location)
import DOM.WebStorage (getItem, getLocalStorage, STORAGE, setItem)
import Data.Exists (mkExists)
import Data.Generic (class Generic)
import Data.StrMap (empty)
import Model.User (SessionDetails)
import Prelude ((>>>), (<$>), (>>=), Unit, bind)
import VirtualDOM (applyPatch, appendToBody, createElement)
import VirtualDOM.HTML (div)
import WebWorker (OwnsWW, mkWorker)
import WebWorker.Channel (postMessageToWorkerC, onmessageFromWorkerC, registerChannel)

main :: forall eff. Eff ( dom :: DOM , ownsww :: OwnsWW , console :: CONSOLE, err :: EXCEPTION, storage :: STORAGE | eff ) Unit
main = do
  -- Initial render
  let initial = div [] []
  node <- createElement initial
  appendToBody node

  -- Start WW
  ww <- mkWorker "worker.js"

  -- Register handlers for incoming messages on channels
  let mdh = mkUIHandlers allEvents ww actionsChannel
  let reg1 = registerChannel empty patchesChannel (\serPs -> applyPatch node serPs mdh)
  let reg2 = registerChannel reg1 sessionsChannel saveSession
  let chs = registerChannel reg2 setHashChannel (\s -> window >>= location >>= \l -> setHash s l)
  onmessageFromWorkerC ww chs

  -- Register onHashChange listener to send hash changes to worker
  onHashChange (getHash >>= HashChanged >>> postMessageToWorkerC ww actionsChannel)

  -- Kick off rendering by sending initial hash to worker
  ms <- getLocalStorage >>= \ls -> getItem ls sessionDetailsKey
  getHash >>= \s -> postMessageToWorkerC ww actionsChannel (Init s ms)

onHashChange :: forall a e. (Eff ( dom :: DOM | e) a) -> Eff ( dom :: DOM | e) Unit
onHashChange evl =
  window >>= \w ->
    addEventListener hashchange (eventListener \_ -> evl) false (windowToEventTarget w)

getHash :: forall e. Eff ( dom :: DOM | e) String
getHash = window >>= location >>= hash

allEvents :: Array EventS
allEvents = [mkExists click, mkExists change, mkExists submit]

----------------------- Storage

saveSession :: forall e.
  SessionDetails -> Eff ( dom :: DOM , storage :: STORAGE | e) Unit
saveSession s = getLocalStorage >>= \ls -> setItem ls sessionDetailsKey s

data StorageKey a = SessionDetailsKey
derive instance genericStorageKey :: Generic (StorageKey a)

sessionDetailsKey :: StorageKey SessionDetails
sessionDetailsKey = SessionDetailsKey
