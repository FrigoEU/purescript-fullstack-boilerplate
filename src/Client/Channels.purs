module Client.Channels where

import VirtualDOM (SerializedVPatches)
import WebWorker.Channel (Channel(Channel))

patchesChannel :: Channel SerializedVPatches
patchesChannel = Channel "serializedvpatchesyo"

setHashChannel :: Channel String
setHashChannel = Channel "setHash"
