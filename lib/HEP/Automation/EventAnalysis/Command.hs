module HEP.Automation.EventAnalysis.Command where

import HEP.Automation.EventAnalysis.ProgType
import HEP.Automation.EventAnalysis.Job

commandLineProcess :: EventAnalysis -> IO ()
commandLineProcess (Single lhefp pdffp) = do 
  putStrLn "test called"
  startSingle lhefp pdffp 