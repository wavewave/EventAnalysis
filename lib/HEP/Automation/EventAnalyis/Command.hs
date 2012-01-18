module HEP.Automation.EventAnalyis.Command where

import HEP.Automation.EventAnalyis.ProgType
import HEP.Automation.EventAnalyis.Job

commandLineProcess :: EventAnalysis -> IO ()
commandLineProcess Test = do 
  putStrLn "test called"
  startJob
