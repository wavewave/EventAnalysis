module HEP.Automation.EventAnalysis.Command where

import HEP.Automation.EventAnalysis.ProgType
import HEP.Automation.EventAnalysis.Job

commandLineProcess :: EventAnalysis -> IO ()
commandLineProcess (Single lhefp pdffp) = do 
  putStrLn "test called"
  startSingle lhefp pdffp 
commandLineProcess (JsonTest fp) = do 
  putStrLn "jsontest called"
  startJsonTest fp
commandLineProcess (MultiAnalysis fp) = do 
  putStrLn "jsontest called"
  startMultiAnalysis fp
commandLineProcess (Junjie lhefp outfp) = do 
  putStrLn "test called"
  startJunjie lhefp outfp 
commandLineProcess (LowMassAnalysis hsfp) = do 
  startLowMassAnalysis hsfp 

