module Server.Main where

import Client.Debug (debug)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log, CONSOLE)
import Control.Monad.Eff.Exception (Error, error)
import Control.Monad.Eff.Now (NOW, now)
import Control.Monad.Error.Class (class MonadError, throwError)
import Data.DateTime.Instant (toDateTime)
import Data.Either (Either(Right, Left))
import Data.Int (fromString)
import Data.Maybe (fromMaybe, maybe, Maybe(Nothing, Just))
import Data.Show (class Show)
import Data.StrMap (StrMap)
import Database.Postgres (DB, Client, ConnectionString, withClient', mkConnectionString)
import Model.User (RegisterError(EmailInUse), SessionDetails(SessionDetails), UserPass(UserPass))
import MyApp.SQL (makeUser, findUser, createSession, validUser)
import Node.Express.Endpoint (Input, compression, Middleware, hostEndpoint, listen, hostStatic, makeApp, EXPRESS)
import Node.Process (lookupEnv, PROCESS)
import Prelude (Unit, pure, bind, show, (<<<), (=<<), (<#>), ($), (<$>), (<>))
import Server.Endpoint (registerUser, loginUser)

foreign import morgan :: String -> Middleware

type ServerEffects = (db :: DB, console :: CONSOLE, express :: EXPRESS, process :: PROCESS, now :: NOW)

main :: Eff ServerEffects Unit
main = do
  port <- fromMaybe 8000 <$> lookupEnvInt "PORT"
  dburi <- fromMaybe localConnInfo <$> lookupEnv "DATABASE_URL"
  let withDb = withClient' dburi :: forall e a. (Client -> Aff (db :: DB | e) a) -> Aff (db :: DB | e) a

  app <- makeApp [compression]
  hostEndpoint app loginUser (logger \_ req -> withDb (\cl -> login' cl req.body))
  hostEndpoint app registerUser (logger \_ req -> withDb (\cl -> register' cl req.body))
  hostStatic app "."
  listen app port
  log $ "Starting server on " <> show port


-- TODO logging:
-- Requiring Show instances for everything is kinda annoying just for logging...
-- Should we also log responses? Same annoyance wrt. Show instances
-- Maybe we should log the raw requests with something like express-winston?
-- Or we dump express all together and just make a nice function composition with logging included?
-- Maybe we should do more structured logging instead of just strings?
logger :: forall qp body eff a.
          (Show body) =>
           (qp -> Input body -> Aff (console :: CONSOLE | eff) a)
           -> (qp -> Input body -> Aff (console :: CONSOLE | eff) a)
logger f = \qp req -> do liftEff $ log $ "url: " <> (req.url) <> "\nbody: " <> show (req.body)
                         f qp req

-- TODO can we improve register' and login'?
register' :: forall e. Client -> UserPass -> Aff ( db :: DB , now :: NOW | e) (Either RegisterError SessionDetails)
register' cl (up@(UserPass {email, password})) = do
  u <- findUser cl {email}
  maybe
    (do makeUser cl {email, password}
        sess <- login' cl up
        maybe (tro "Internal Error: Failed to login new user") (pure <<< Right) sess
    )
    (\_ -> pure (Left EmailInUse))
    u

login' :: forall e. Client -> UserPass -> Aff (db :: DB, now :: NOW | e) (Maybe SessionDetails)
login' cl (UserPass {email, password}) = do
  n <- liftEff $ toDateTime <$> now
  mui <- validUser cl {email: email, password: password}
  maybe
    (pure Nothing)
    (\({userId}) -> do
        ms <- createSession cl {userId, now: n}
        maybe
          (tro "Internal Error: Failed to login user") -- TODO add HTTP status code support
          (\({sessionId}) -> pure $ Just $ SessionDetails {sessionId, userId, email})
          ms
    ) mui

localConnInfo :: ConnectionString
localConnInfo = mkConnectionString { host: "localhost"
                                   , db: "myapp"
                                   , port: 5432
                                   , user: ""
                                   , password: ""
                                   }

lookupEnvInt :: forall e. String -> Eff ( process :: PROCESS | e) (Maybe Int)
lookupEnvInt str = lookupEnv str <#> (=<<) fromString

tro :: forall m a. (MonadError Error m) => String -> m a
tro = throwError <<< error
