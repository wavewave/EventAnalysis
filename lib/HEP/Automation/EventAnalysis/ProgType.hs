{-# LANGUAGE DeriveDataTypeable #-}

module HEP.Automation.EventAnalysis.ProgType where 

import System.Console.CmdArgs

data EventAnalysis = Single { datafilename :: FilePath 
                            , outputpdffilename :: FilePath 
                            }
                   | JsonTest { jsonfilename :: FilePath } 
                   | MultiAnalysis { jsonfilename :: FilePath }
              deriving (Show,Data,Typeable)

single :: EventAnalysis
single = Single { datafilename = def &= typ "LHEFILE" &= argPos 0 
                , outputpdffilename = def &= typ "PDFFILE" &= argPos 1 
                } 

jsontest :: EventAnalysis
jsontest = JsonTest { jsonfilename = def &= typ "JSONFILE" &= argPos 0 } 

multianalysis :: EventAnalysis
multianalysis = MultiAnalysis { jsonfilename = def &= typ "JSONFILE" &= argPos 0 }


mode :: EventAnalysis
mode = modes [single, jsontest, multianalysis]

