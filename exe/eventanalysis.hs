module Main where

import System.Console.CmdArgs

import HEP.Automation.EventAnalysis.ProgType
import HEP.Automation.EventAnalysis.Command

main :: IO () 
main = do 
  putStrLn "EventAnalysis"
  param <- cmdArgs mode

  commandLineProcess param