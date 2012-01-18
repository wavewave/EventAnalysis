{-# LANGUAGE DeriveDataTypeable #-}

module HEP.Automation.EventAnalyis.ProgType where 

import System.Console.CmdArgs

data EventAnalysis = Test 
              deriving (Show,Data,Typeable)

test :: EventAnalysis
test = Test 

mode = modes [test]

