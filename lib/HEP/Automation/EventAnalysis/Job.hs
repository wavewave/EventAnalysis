
module HEP.Automation.EventAnalysis.Job where

import HROOT
import HEP.Automation.EventAnalysis.FileDriver 
import HEP.Automation.EventAnalysis.Print 

import Control.Monad.Trans

import Data.IORef

import HEP.Physics.TTBar.Analysis.TopPairParton


startSingle :: FilePath -> FilePath -> IO () 
startSingle lhefile pdffile = do 
  putStrLn "job started"
  putStrLn $ lhefile 
  putStrLn $ pdffile 
  
  h1 <- newTH1D "test" "test" 50 (-1.2) 1.2 

  ref <- newIORef ((0,0) :: (Int,Int)) 

  let analysis = SingleFileAnalysisDraw1DHistFromLHE { datafile = lhefile 
                                                     , hist1d = h1 
                                                     , hist1dfunc = const (countTTBarFBUsingIORef ref)
                                                                    -- const showNonTTBarEvent
                                                                    -- const (countTTBarEventUsingIORef ref)
                                                                    -- const (showTTBarEvent)
                                                                    -- const (showSomeEvents 30) 
                                                                    -- const (return ())
                                                     }
            
  doSingleFileAnalysis analysis 

  -- doReadXmlOnly "ttbarevents.lhe"

  n <- readIORef ref 
  putStrLn $ " (f,b) = " ++ show n 