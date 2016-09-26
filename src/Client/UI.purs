module Client.UI where

import Client.WEvents (allWEvents)
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
import Data.StrMap (empty)
import Prelude ((<$>), (>>=), Unit, bind)
import VirtualDOM (applyPatch, appendToBody, createElement, props, div, SerializedVPatches)
import VirtualDOM.Worker (mkDeserializeHandlersForWEvents, WEventMessage)
import WebWorker (OwnsWW, mkWorker)
import WebWorker.Channel (postMessageToWorkerC, onmessageFromWorkerC, registerChannel, Channel(Channel))

main :: forall eff. Eff ( dom :: DOM , ownsww :: OwnsWW , console :: CONSOLE, err :: EXCEPTION | eff ) Unit
main = do
  let initial = div (props []) []
  node <- createElement initial
  appendToBody node
  ww <- mkWorker "worker.js"
  let mdh = mkDeserializeHandlersForWEvents ww wEventsChannel allWEvents
  let reg1 = registerChannel empty patchesChannel (\serPs -> applyPatch node serPs mdh)
  let chs = registerChannel reg1 setHashChannel (\s -> window >>= location >>= \l -> setHash s l)
  window >>= \w -> addEventListener
                   hashchange
                   (eventListener (\_ -> postMessageToWorkerC ww hashChangedChannel <$> (window >>= location >>= hash)))
                   false
                   (windowToEventTarget w)
  onmessageFromWorkerC ww chs
  startingHash <- window >>= location >>= hash
  postMessageToWorkerC ww hashChangedChannel startingHash

patchesChannel :: Channel SerializedVPatches
patchesChannel = Channel "serializedvpatchesyo"

wEventsChannel :: Channel WEventMessage
wEventsChannel = Channel "weventsmessagesfam"

setHashChannel :: Channel String
setHashChannel = Channel "setHash"

hashChangedChannel :: Channel String
hashChangedChannel = Channel "hashChanged"
