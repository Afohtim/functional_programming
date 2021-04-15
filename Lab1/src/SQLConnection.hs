{-# LANGUAGE OverloadedStrings #-}
module SQLConnection where

import Data.Text as T ( Text )
import Database.MySQL.Base
import qualified System.IO.Streams as Streams
import qualified Data.ByteString.Lazy.Char8 as BtSt ( pack )

-- connect to database which is started by docker-compose
connectDB :: IO MySQLConn
connectDB =
    connect
        defaultConnectInfo { ciPort = 3306,
                             ciUser = "admin",
                             ciPassword = "password",
                             ciDatabase = "haskell_sports"
                            }

-- close connection to database
closeDB :: MySQLConn -> IO ()
closeDB = close

-- translate output results into list of values
getRidOfStream :: IO ([ColumnDef], Streams.InputStream [MySQLValue]) -> IO [[MySQLValue]]
getRidOfStream all = do
    (defs, is) <- all
    Streams.toList is

-- get name of database
getDBName :: MySQLConn -> IO [[MySQLValue]]
getDBName conn = getRidOfStream (query_ conn "SELECT DATABASE();")
