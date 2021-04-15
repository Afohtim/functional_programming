{-# LANGUAGE OverloadedStrings #-}
module Table where

import Data.Text as T ( Text )
import Database.MySQL.Base
import qualified System.IO.Streams as Streams
import qualified Data.ByteString.Lazy.Char8 as BtSt ( pack )

import SQLConnection

showTables :: MySQLConn -> IO [[MySQLValue]]
showTables conn = getRidOfStream (query_ conn "SHOW TABLES;")

toQuery :: String -> Query
toQuery = Query . BtSt.pack

errorOnNotExistence :: OK
errorOnNotExistence = OK (-100) (-100) 0 0

errorOnExistence :: OK
errorOnExistence = OK (-200) (-200) 0 0

generateWhere :: [String] -> String
generateWhere [] = ""
generateWhere [x] = x ++ " = ?"
generateWhere (x:xs) = x ++ " = ? and " ++ generateWhere xs

generateSet :: [String] -> String
generateSet [] = ""
generateSet [x] = x ++ " = ?"
generateSet (x:xs) = x ++ " = ?, " ++ generateSet xs

generateInsert :: [String] -> String
generateInsert [] = " "
generateInsert [x] = x
generateInsert (x:xs) = x ++ ", " ++ generateInsert xs

generateInsertValues :: Int -> String
generateInsertValues n
    | n == 0 = " "
    | n == 1 = "?"
    | n > 1  = "?, " ++ generateInsertValues (n-1)



class Table a where
    getName :: a -> String 
    getFieldNames :: a -> [String] 
    getFieldValues :: a -> [MySQLValue]
    getMainFieldTables :: a -> a
    fromMySQLValues :: IO [[MySQLValue]] -> IO a
    isEmpty :: a -> Bool    
    len :: a -> Int

    readValue :: a -> MySQLConn -> IO a
    readValue tableInfo conn = 
        fromMySQLValues (getRidOfStream (query conn (
            toQuery ("SELECT * FROM " ++ getName tableInfo ++ 
                     " WHERE " ++ generateWhere (getFieldNames tableInfo)))
            (getFieldValues tableInfo)))

    readAllValues :: a -> MySQLConn -> IO a
    readAllValues tableInfo conn =
        fromMySQLValues (getRidOfStream ( query_ conn (toQuery("SELECT * FROM " ++ getName tableInfo ++ ";"))))

    updateValue :: a -> a -> MySQLConn -> IO OK
    updateValue newValue oldValue conn = do
        vals <- readValue oldValue conn
        if isEmpty vals
        then return errorOnNotExistence
        else
            execute conn (toQuery (
            "UPDATE " ++ getName newValue ++ " \
            \SET " ++ generateSet (getFieldNames newValue) ++ " \
            \WHERE " ++ generateWhere (getFieldNames oldValue) ++ ";"))
            (getFieldValues newValue ++ getFieldValues oldValue)
        


    addValue :: a -> MySQLConn -> IO OK
    addValue tableInfo conn = do
        vals <- readValue (getMainFieldTables tableInfo) conn        
        if isEmpty vals
        then execute conn
                (toQuery ("INSERT INTO " ++ getName tableInfo ++ 
                            " (" ++ generateInsert (getFieldNames tableInfo) ++ ") " ++
                        "VALUES (" ++ generateInsertValues (len tableInfo) ++ ") ;"))
                (getFieldValues tableInfo)
        else return errorOnExistence

    deleteValue :: a -> MySQLConn -> IO OK
    deleteValue tableInfo conn = 
        execute conn (toQuery ("DELETE FROM " ++ getName tableInfo ++ 
                                " WHERE " ++ generateWhere (getFieldNames tableInfo) ++ ";")) 
                                (getFieldValues tableInfo)

    

    