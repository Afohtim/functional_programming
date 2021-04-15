module Main where

import Table
import Students
import SQLConnection
import CheckTables


main :: IO ()
main = do
    conn <- connectDB
    checkAllTables conn
    closeDB conn
