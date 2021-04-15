module SportSection where 


import qualified Data.Text as T ( Text, pack )
import Database.MySQL.Base
import Data.Int (Int32)
import qualified Text.PrettyPrint as TPrettyP ( ($+$), text, vcat, Doc, (<>), render )

import SQLConnection
import Table
import Converter


data SportSectionInfo = SportSectionInfo{
        tableName :: String,
        fieldNames :: [String],
        ids :: [Int32],
        names :: [String]
    } deriving Show



emptySportSectionInstance :: SportSectionInfo
emptySportSectionInstance = SportSectionInfo "Sport_section" ["id", "name"] [] []

instance Table SportSectionInfo where
    getName tableInfo = tableName tableInfo

    getFieldNames tableInfo = [fieldNames tableInfo !! 0 | not (null (ids tableInfo))] ++
                        [fieldNames tableInfo !! 1 | not (null (names tableInfo))]

    getFieldValues (SportSectionInfo _ _ ids names) =
        map MySQLInt32 ids ++
        map (MySQLText . T.pack) names

    getMainFieldTables tableInfo = SportSectionInfo {
            tableName = tableName tableInfo,
            fieldNames = fieldNames tableInfo,
            ids = [],
            names = names tableInfo
        }
    
    isEmpty tableInfo = null (ids tableInfo) || null (names tableInfo)

    len tableInfo = fromEnum (not (null (ids tableInfo))) +
                    fromEnum (not (null (names tableInfo)))


     

    fromMySQLValues res = do
        vals <- res
        return (SportSectionInfo {
            tableName = tableName emptySportSectionInstance,
            fieldNames = fieldNames emptySportSectionInstance,
            ids = map myToInt32 (genStruct vals 0),
            names = map myToString (genStruct vals 1)
        })

    