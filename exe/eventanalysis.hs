module Main where

import System.Console.CmdArgs

import HEP.Automation.EventAnalyis.ProgType
import HEP.Automation.EventAnalyis.Command

main :: IO () 
main = do 
  putStrLn "EventAnalysis"
  param <- cmdArgs mode

  commandLineProcess param