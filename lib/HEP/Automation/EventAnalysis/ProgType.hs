{-# LANGUAGE DeriveDataTypeable #-}

module HEP.Automation.EventAnalysis.ProgType where 

import System.Console.CmdArgs

data EventAnalysis = Single { datafilename :: FilePath 
                            , outputpdffilename :: FilePath 
                            }
                   | JsonTest
              deriving (Show,Data,Typeable)

single :: EventAnalysis
single = Single { datafilename = def &= typ "LHEFILE" &= argPos 0 
                , outputpdffilename = def &= typ "PDFFILE" &= argPos 1 
                } 

jsontest :: EventAnalysis
jsontest = JsonTest 

mode :: EventAnalysis
mode = modes [single, jsontest]

