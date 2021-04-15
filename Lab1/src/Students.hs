module Students where 


import qualified Data.Text as T ( Text, pack )
import Database.MySQL.Base
import Data.Int (Int32)
import qualified Text.PrettyPrint as TPrettyP ( ($+$), text, vcat, Doc, (<>), render )

import SQLConnection
import Table
import Converter


data StudentsInfo = StudentsInfo{
        tableName :: String,
        fieldNames :: [String],
        ids :: [Int32],
        names :: [String],
        surnames :: [String]
    } deriving Show



emptyStudentsInstance :: StudentsInfo
emptyStudentsInstance = StudentsInfo "Students" ["id", "name", "surname"] [] [] []

instance Table StudentsInfo where
    getName tableInfo = tableName tableInfo

    getFieldNames tableInfo = [fieldNames tableInfo !! 0 | not (null (ids tableInfo))] ++
                        [fieldNames tableInfo !! 1 | not (null (names tableInfo))] ++
                        [fieldNames tableInfo !! 2 | not (null (surnames tableInfo))]

    getFieldValues (StudentsInfo _ _ ids names surnames) =
        map MySQLInt32 ids ++
        map (MySQLText . T.pack) names ++
        map (MySQLText . T.pack) surnames

    getMainFieldTables tableInfo = StudentsInfo {
            tableName = tableName tableInfo,
            fieldNames = fieldNames tableInfo,
            ids = [],
            names = names tableInfo,
            surnames = surnames tableInfo
        }
    
    isEmpty tableInfo = null (ids tableInfo) || null (names tableInfo) || 
                        null (surnames tableInfo)

    len tableInfo = fromEnum (not (null (ids tableInfo))) +
                    fromEnum (not (null (names tableInfo))) +
                    fromEnum (not (null (surnames tableInfo)))


     

    fromMySQLValues res = do
        vals <- res
        return (StudentsInfo {
            tableName = tableName emptyStudentsInstance,
            fieldNames = fieldNames emptyStudentsInstance,
            ids = map myToInt32 (genStruct vals 0),
            names = map myToString (genStruct vals 1),
            surnames = map myToString (genStruct vals 2)
        })

    