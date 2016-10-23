module Model.UUID where

import Data.Generic (class Generic)
import Database.Postgres.SqlValue (fromSql, toSql, class IsSqlValue)
import Prelude ((<$>))

newtype UUID = UUID String

derive instance genericUUID :: Generic UUID
instance isSqlValueUUID :: IsSqlValue UUID where
  toSql (UUID s) = toSql s
  fromSql o = UUID <$> fromSql o
