
module HEP.Automation.EventAnalysis.Job where

import HROOT
import HEP.Automation.EventAnalysis.FileDriver 
import HEP.Automation.EventAnalysis.Print 

import Control.Monad.Trans


import HEP.Physics.TTBar.Analysis.TopPairParton


startSingle :: FilePath -> FilePath -> IO () 
startSingle lhefile pdffile = do 
  putStrLn "job started"
  putStrLn $ lhefile 
  putStrLn $ pdffile 
  
  h1 <- newTH1D "test" "test" 50 (-1.2) 1.2 

  let analysis = SingleFileAnalysisDraw1DHistFromLHE { datafile = lhefile 
                                                     , hist1d = h1 
                                                     , hist1dfunc = const (showTTBarEvent)
                                                                    -- const (showSomeEvents 3) 
                                                                    -- const (return ())
                                                     }
            
  doSingleFileAnalysis analysis 
