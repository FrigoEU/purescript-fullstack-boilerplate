module MyApp.SQL where
import Prelude ((<$>), map, (<*>))
import Database.Postgres (Client, DB, query, Query(Query), queryOne)
import Control.Monad.Aff (Aff)
import Data.Maybe (Maybe)
import Data.Foreign (F)
import Database.Postgres.SqlValue (toSql, readSqlProp, fromSql, class IsSqlValue)
import Model.UUID (UUID(UUID))
import Model.User (SessionId(SessionId), UserId(UserId))
import Data.DateTime (DateTime(DateTime))

makeUser :: forall eff obj. Client -> {email :: String, password :: String | obj} -> Aff (db :: DB | eff) (Maybe {userId :: UserId})
makeUser cl {email, password} = (map runRes0) <$> queryOne (Query "select * from makeUser($1,$2)") [toSql email, toSql password] cl
newtype Res0 = Res0 {userId :: UserId}
runRes0 :: Res0 -> {userId :: UserId}
runRes0 (Res0 a) = a
instance isSqlValueRes0 :: IsSqlValue Res0 where 
 toSql a = toSql ""
 fromSql obj = Res0 <$> ({userId: _} <$> (UserId <$> (readSqlProp "userid" obj :: F UUID)))

findUser :: forall eff obj. Client -> {email :: String | obj} -> Aff (db :: DB | eff) (Maybe {userId :: UserId})
findUser cl {email} = (map runRes1) <$> queryOne (Query "select * from findUser($1)") [toSql email] cl
newtype Res1 = Res1 {userId :: UserId}
runRes1 :: Res1 -> {userId :: UserId}
runRes1 (Res1 a) = a
instance isSqlValueRes1 :: IsSqlValue Res1 where 
 toSql a = toSql ""
 fromSql obj = Res1 <$> ({userId: _} <$> (UserId <$> (readSqlProp "userid" obj :: F UUID)))

validUser :: forall eff obj. Client -> {email :: String, password :: String | obj} -> Aff (db :: DB | eff) (Maybe {userId :: UserId})
validUser cl {email, password} = (map runRes2) <$> queryOne (Query "select * from validUser($1,$2)") [toSql email, toSql password] cl
newtype Res2 = Res2 {userId :: UserId}
runRes2 :: Res2 -> {userId :: UserId}
runRes2 (Res2 a) = a
instance isSqlValueRes2 :: IsSqlValue Res2 where 
 toSql a = toSql ""
 fromSql obj = Res2 <$> ({userId: _} <$> (UserId <$> (readSqlProp "userid" obj :: F UUID)))

createSession :: forall eff obj. Client -> {userId :: UserId, now :: DateTime | obj} -> Aff (db :: DB | eff) (Maybe {sessionId :: SessionId})
createSession cl {userId: (UserId userId), now} = (map runRes3) <$> queryOne (Query "select * from createSession($1,$2)") [toSql userId, toSql now] cl
newtype Res3 = Res3 {sessionId :: SessionId}
runRes3 :: Res3 -> {sessionId :: SessionId}
runRes3 (Res3 a) = a
instance isSqlValueRes3 :: IsSqlValue Res3 where 
 toSql a = toSql ""
 fromSql obj = Res3 <$> ({sessionId: _} <$> (SessionId <$> (readSqlProp "sessionid" obj :: F UUID)))

getSession :: forall eff obj. Client -> {sessionId :: SessionId | obj} -> Aff (db :: DB | eff) (Maybe {id :: SessionId, userId :: UserId, createdAt :: DateTime})
getSession cl {sessionId: (SessionId sessionId)} = (map runRes4) <$> queryOne (Query "select * from getSession($1)") [toSql sessionId] cl
newtype Res4 = Res4 {id :: SessionId, userId :: UserId, createdAt :: DateTime}
runRes4 :: Res4 -> {id :: SessionId, userId :: UserId, createdAt :: DateTime}
runRes4 (Res4 a) = a
instance isSqlValueRes4 :: IsSqlValue Res4 where 
 toSql a = toSql ""
 fromSql obj = Res4 <$> ({id: _, userId: _, createdAt: _} <$> (SessionId <$> (readSqlProp "id" obj :: F UUID)) <*> (UserId <$> (readSqlProp "userid" obj :: F UUID)) <*> (readSqlProp "createdat" obj :: F DateTime))

