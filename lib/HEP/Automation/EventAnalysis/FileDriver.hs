{-# LANGUAGE ScopedTypeVariables, RecordWildCards, Rank2Types, ExistentialQuantification #-}

module HEP.Automation.EventAnalysis.FileDriver where

import Control.Monad.Trans

import Control.Monad.State

-- import Data.Enumerator 
import Data.Enumerator 
import Data.Enumerator.Util
import Data.Enumerator.Util.Count
import HEP.Util.Count
import System.IO
import Data.XML.Types

import HEP.Parser.LHEParser.Parser.Enumerator 
import HEP.Parser.LHEParser.Type
import HEP.Parser.LHEParser.DecayTop

import Text.XML.Enumerator.Parse.Util

import HEP.Automation.EventAnalysis.Print

import HROOT


data SingleFileAnalysis =  SingleFileAnalysisDraw1DHistFromLHE 
                           { datafile :: FilePath
                           , hist1d :: TH1D
                           , hist1dfunc :: forall a b m. (Show a, MonadIO m) => 
                                           TH1D -> Iteratee (Maybe (a,b,[DecayTop PtlIDInfo])) m () 
                           }

processFile :: (Iteratee Event CountIO a) -> FilePath -> IO (a,Int)
processFile iter fp = do 
  putStrLn $ "process " ++ fp   
  withFile fp ReadMode $ \ih -> runStateT (parseXmlFile ih iter) (0::Int)
                  


doSingleFileAnalysis :: SingleFileAnalysis -> IO ()
doSingleFileAnalysis SingleFileAnalysisDraw1DHistFromLHE{..} = do 
  let process = enumZip3 countIter countMarkerIter (hist1dfunc hist1d)  
  let iter = do 
         header <- textLHEHeader
         parseEventIter $ decayTopEnee =$ ordDecayTopEnee =$ process
  r <- processFile iter datafile
  putStrLn $ show r 
  return ()

doReadXmlOnly :: FilePath -> IO ()
doReadXmlOnly fp = do 
    let -- process :: Double
        process = enumZip3 countIter countMarkerIter (showSomeEvents 30) -- (hist1dfunc hist1d)  
        -- test :: Double = parseEventIter
    let iter = do 
          header <- textLHEHeader
          parseEventIter $ decayTopEnee =$ process
    r <- processFile iter fp 
    putStrLn $ show r 
    return ()

