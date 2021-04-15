#!/usr/bin/env stack

module Main where

import Gaussian 
import Control.Concurrent
import GHC.Conc ( numCapabilities, pseq )
import System.Environment (getArgs)
import GHC.Conc.Sync (par)
import Text.Printf (printf)
import System.Random (StdGen, getStdGen, randoms)
import Data.Time.Clock (diffUTCTime, getCurrentTime)


main :: IO ()
main = do
  let size = 30    
  putStrLn "Parallel"
  (mat1, vec1) <- generateRandom size
  start <- getCurrentTime
  let ans = gaussian mat1 vec1 True
  let x = case ans of
        Exists vec -> vec
        _ -> []
  let y = force x
  print y
  end <- getCurrentTime
  putStrLn $ show (end `diffUTCTime` start) ++ " elapsed."
  putStrLn $ "number of cores: " ++ show numCapabilities

  putStrLn "Sequential"
  (mat1, vec1) <- generateRandom size
  start <- getCurrentTime
  let ans = gaussian mat1 vec1 False
  let x = case ans of
        Exists vec -> vec
        _ -> []
  let y = force x
  print y
  end <- getCurrentTime
  putStrLn $ show (end `diffUTCTime` start) ++ " elapsed."
  putStrLn "number of cores: 1"
