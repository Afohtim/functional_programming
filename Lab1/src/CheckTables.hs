module CheckTables where

import Database.MySQL.Base

import Table
import Students
import Teachers
import SportSection
import TrainingSchedule
import Competition
import SQLConnection

checkStudents :: MySQLConn -> IO ()
checkStudents conn = do
    res <- addValue (StudentsInfo "Students" ["id", "name", "surname"] [] ["l"] ["m"]) conn
    print (res)

    res <- readAllValues emptyStudentsInstance conn
    print (res)

    res <- readValue (StudentsInfo "Students" ["id", "name", "surname"] [] ["l"] ["m"]) conn
    print res

    res <- updateValue (StudentsInfo "Students" ["id", "name", "surname"] [] ["u"] ["v"]) (StudentsInfo "Students" ["id", "name", "surname"] [] ["l"] ["m"]) conn
    print res

    res <- readAllValues emptyStudentsInstance conn
    print res

    deleteValue (StudentsInfo "Students" ["id", "name", "surname"] [] ["u"] ["v"]) conn
    print res

    res <- readAllValues emptyStudentsInstance conn
    print res

checkTeachers :: MySQLConn -> IO ()
checkTeachers conn = do
    res <- addValue (TeachersInfo "Teachers" ["id", "name", "surname"] [] ["l"] ["m"]) conn
    print (res)

    res <- readAllValues emptyTeachersInstance conn
    print (res)

    res <- readValue (TeachersInfo "Teachers" ["id", "name", "surname"] [] ["l"] ["m"]) conn
    print res

    res <- updateValue (TeachersInfo "Teachers" ["id", "name", "surname"] [] ["u"] ["v"]) (TeachersInfo "Teachers" ["id", "name", "surname"] [] ["l"] ["m"]) conn
    print res

    res <- readAllValues emptyTeachersInstance conn
    print res

    deleteValue (TeachersInfo "Teachers" ["id", "name", "surname"] [] ["u"] ["v"]) conn
    print res

    res <- readAllValues emptyTeachersInstance conn
    print res


checkSportSection :: MySQLConn -> IO ()
checkSportSection conn = do
    res <- addValue (SportSectionInfo "Sport_section" ["id", "name"] [] ["s"]) conn
    print (res)

    res <- readAllValues emptySportSectionInstance conn
    print (res)

    res <- readValue (SportSectionInfo "Sport_section" ["id", "name"] [] ["s"]) conn
    print res

    res <- updateValue (SportSectionInfo "Sport_section" ["id", "name"] [] ["s_s"]) (SportSectionInfo "Sport_section" ["id", "name"] [] ["s"]) conn
    print res

    res <- readAllValues emptySportSectionInstance conn
    print res

    res <- deleteValue (SportSectionInfo "Sport_section" ["id", "name"] [] ["s_s"]) conn
    print res

    res <- readAllValues emptySportSectionInstance conn
    print res

checkTrainingSchedule :: MySQLConn -> IO ()
checkTrainingSchedule conn = do
    res <- readAllValues emptyTrainingScheduleInstance conn
    print (res)

    res <- addValue newData  conn
    print (res)

    res <- readAllValues emptyTrainingScheduleInstance conn
    print (res)

    res <- readValue newData conn
    print res

    res <- updateValue updatedData newData conn
    print res

    res <- readAllValues emptyTrainingScheduleInstance conn
    print res

    res <- deleteValue updatedData conn
    print res

    res <- readAllValues emptyTrainingScheduleInstance conn
    print res

    where
        newData = (TrainingScheduleInfo "Training_schedule" ["id", "section_id", "teacher_id", "weekday",  "time_start", "time_end"] [] [2] [2] ["Friday"] ["12:30:00"] ["14:05:00"])
        updatedData = (TrainingScheduleInfo "Training_schedule" ["id", "section_id", "teacher_id", "weekday", "time_start", "time_end"] [] [2] [2] ["Wednesday"] ["12:35:00"] ["14:10:00"])

    
checkCompetition :: MySQLConn -> IO ()
checkCompetition conn = do
    res <- readAllValues emptyCompetitionInstance conn
    print (res)

    res <- addValue newData  conn
    print (res)

    res <- readAllValues emptyCompetitionInstance conn
    print (res)

    res <- readValue newData conn
    print res

    res <- updateValue updatedData newData conn
    print res

    res <- readAllValues emptyCompetitionInstance conn
    print res

    res <- deleteValue updatedData conn
    print res

    res <- readAllValues emptyCompetitionInstance conn
    print res

    where
        newData = (CompetitionInfo "Competition" ["id", "name", "time_start", "time_end"]) [] ["competition"] ["2021-11-11 11:10:00"] ["2021-11-15 15:10:00"]
        updatedData = (CompetitionInfo "Competition" ["id", "name", "time_start", "time_end"]) [] ["competition_rerun"] ["2021-11-15 11:10:00"] ["2021-11-20 15:10:00"]




checkAllTables :: MySQLConn -> IO ()
checkAllTables conn = do 
    checkStudents conn
    checkTeachers conn
    checkSportSection conn
    checkTrainingSchedule conn
    checkCompetition conn
    