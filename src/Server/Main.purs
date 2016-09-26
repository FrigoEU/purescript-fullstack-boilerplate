module Server.Main where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Database.Postgres (DB)
import Node.Express.Endpoint (listen, hostStatic, makeApp, EXPRESS)
import Prelude (show, (<>), ($), Unit, bind)

-- TODO: HOE DOET 12 FACTOR DIT?
-- localConnInfo :: ConnectionInfo
-- localConnInfo = {host: "localhost", db: "imgoing", port: 5432, user: "", password: ""}

-- withDb :: forall a eff. (Client -> Aff (db :: DB | eff) a) -> Aff (db :: DB | eff) a
-- withDb = withClient localConnInfo

-- TODO: HOE DOET 12 FACTOR DIT?
port :: Int
port = 8080

main :: forall eff. Eff (db :: DB, console :: CONSOLE, express :: EXPRESS | eff) Unit
main = do
  app <- makeApp
  -- hostEndpoint app registerUser
  --   (\_ _ -> withDb (\cl -> TODO))
  -- hostEndpoint app loginUser
  --   (\_ _ -> withDb (\cl -> TODO))
  hostStatic app "static"
  listen app port
  log $ "Starting server on " <> show port
