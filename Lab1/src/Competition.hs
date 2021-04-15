module Competition where 


import qualified Data.Text as T ( Text, pack )
import Database.MySQL.Base
import Data.Int (Int32)
import qualified Text.PrettyPrint as TPrettyP ( ($+$), text, vcat, Doc, (<>), render )

import SQLConnection
import Table
import Converter


data CompetitionInfo = CompetitionInfo{
        tableName :: String,
        fieldNames :: [String],
        ids :: [Int32],
        names :: [String],
        timeStart :: [String],
        timeEnd :: [String]
    } deriving Show



emptyCompetitionInstance :: CompetitionInfo
emptyCompetitionInstance = CompetitionInfo "Competition" ["id", "name", "time_start", "time_end"] [] [] [] []

instance Table CompetitionInfo where
    getName tableInfo = tableName tableInfo

    getFieldNames tableInfo = [fieldNames tableInfo !! 0 | not (null (ids tableInfo))] ++
                        [fieldNames tableInfo !! 1 | not (null (names tableInfo))] ++
                        [fieldNames tableInfo !! 2 | not (null (timeStart tableInfo))] ++
                        [fieldNames tableInfo !! 3 | not (null (timeEnd tableInfo))]

    getFieldValues (CompetitionInfo _ _ ids names timeStart timeEnd) =
        map MySQLInt32 ids ++
        map (MySQLText . T.pack) names ++
        map (MySQLText . T.pack) timeStart ++
        map (MySQLText . T.pack) timeEnd

    getMainFieldTables tableInfo = CompetitionInfo {
            tableName = tableName tableInfo,
            fieldNames = fieldNames tableInfo,
            ids = [],
            names = names tableInfo,
            timeStart = timeStart tableInfo,
            timeEnd = timeEnd tableInfo
        }
    
    isEmpty tableInfo = null (ids tableInfo) || null (names tableInfo) || 
                        null (timeStart tableInfo) || null (timeEnd tableInfo)

    len tableInfo = fromEnum (not (null (ids tableInfo))) +
                    fromEnum (not (null (names tableInfo))) +
                    fromEnum (not (null (timeStart tableInfo))) +
                    fromEnum (not (null (timeEnd tableInfo)))


     

    fromMySQLValues res = do
        vals <- res
        return (CompetitionInfo {
            tableName = tableName emptyCompetitionInstance,
            fieldNames = fieldNames emptyCompetitionInstance,
            ids = map myToInt32 (genStruct vals 0),
            names = map myToString (genStruct vals 1),
            timeStart = map myToString (genStruct vals 2),
            timeEnd = map myToString (genStruct vals 3)
        })

    