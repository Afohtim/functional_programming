module TrainingSchedule where 


import qualified Data.Text as T ( Text, pack )
import Database.MySQL.Base
import Data.Int (Int32)
import qualified Text.PrettyPrint as TPrettyP ( ($+$), text, vcat, Doc, (<>), render )

import SQLConnection
import Table
import Converter


data TrainingScheduleInfo = TrainingScheduleInfo{
        tableName :: String,
        fieldNames :: [String],
        ids :: [Int32],
        sectionIds :: [Int32],
        teacherIds :: [Int32],
        weekday :: [String],
        timeStart :: [String],
        timeEnd :: [String]

    } deriving Show



emptyTrainingScheduleInstance :: TrainingScheduleInfo
emptyTrainingScheduleInstance = TrainingScheduleInfo "Training_schedule" ["id", "section_id", "teacher_id", "weekday", "time_start", "time_end"] [] [] [] [] [] []

instance Table TrainingScheduleInfo where
    getName tableInfo = tableName tableInfo

    getFieldNames tableInfo = [fieldNames tableInfo !! 0 | not (null (ids tableInfo))] ++
                        [fieldNames tableInfo !! 1 | not (null (sectionIds tableInfo))] ++ 
                        [fieldNames tableInfo !! 2 | not (null (teacherIds tableInfo))] ++
                        [fieldNames tableInfo !! 3 | not (null (weekday tableInfo))] ++
                        [fieldNames tableInfo !! 4 | not (null (timeStart tableInfo))] ++
                        [fieldNames tableInfo !! 5 | not (null (timeEnd tableInfo))]

    getFieldValues (TrainingScheduleInfo _ _ ids sectionIds teacherIds weekday timeStart timeEnd) =
        map MySQLInt32 ids ++
        map MySQLInt32 sectionIds ++
        map MySQLInt32 teacherIds ++
        map (MySQLText . T.pack) weekday ++
        map (MySQLText . T.pack) timeStart ++
        map (MySQLText . T.pack) timeEnd

    getMainFieldTables tableInfo = TrainingScheduleInfo {
            tableName = tableName tableInfo,
            fieldNames = fieldNames tableInfo,
            ids = [],
            sectionIds = sectionIds tableInfo,
            teacherIds = teacherIds tableInfo,
            weekday = weekday tableInfo,
            timeStart = timeStart tableInfo,
            timeEnd = timeEnd tableInfo
        }
    
    isEmpty tableInfo = null (ids tableInfo) || null (sectionIds tableInfo) ||
                        null (sectionIds tableInfo) || null (teacherIds tableInfo) ||
                        null (weekday tableInfo) || null (timeStart tableInfo) || null (timeEnd tableInfo)

    len tableInfo = fromEnum (not (null (ids tableInfo))) +
                    fromEnum (not (null (sectionIds tableInfo))) +
                    fromEnum (not (null (teacherIds tableInfo))) +
                    fromEnum (not (null (weekday tableInfo))) +
                    fromEnum (not (null (timeStart tableInfo))) +
                    fromEnum (not (null (timeEnd tableInfo)))
                    


     

    fromMySQLValues res = do
        vals <- res
        return (TrainingScheduleInfo {
            tableName = tableName emptyTrainingScheduleInstance,
            fieldNames = fieldNames emptyTrainingScheduleInstance,
            ids = map myToInt32 (genStruct vals 0),
            sectionIds = map myToInt32 (genStruct vals 1),
            teacherIds = map myToInt32 (genStruct vals 2),
            weekday = map myToString (genStruct vals 3),
            timeStart = map myToString (genStruct vals 4),
            timeEnd = map myToString (genStruct vals 5)
        })

    