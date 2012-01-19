{-# LANGUAGE ScopedTypeVariables, RecordWildCards, Rank2Types, ExistentialQuantification #-}

module HEP.Automation.EventAnalysis.FileDriver where

import Control.Monad.Trans

import Control.Monad.State

-- import Data.Enumerator 
import Data.Enumerator as E
import Data.Enumerator.IO
import Data.Enumerator.Util
import Data.Enumerator.Util.Count
import Data.Enumerator.Trans
import HEP.Util.Count
import System.IO
import Data.XML.Types

import Text.XML.Stream.Parse

import HEP.Parser.LHEParser.Parser.Enumerator 
import HEP.Parser.LHEParser.Type
import HEP.Parser.LHEParser.DecayTop

import Text.XML.Enumerator.Parse.Util

import HEP.Automation.EventAnalysis.Print

import Codec.Zlib.Enum


import HROOT


data SingleFileAnalysis =  
  SingleFileAnalysisDraw1DHistFromLHE 
  { datafile :: FilePath
  , hist1d :: TH1D
  , hist1dfunc :: forall a b m. (Show a, MonadIO m) => 
                  TH1D -> Iteratee (Maybe (a,b,[DecayTop PtlIDInfo])) m () 
  }
  | SingleFileAnalysisCountingLHE
  { datafile :: FilePath
  , countfunc :: forall a b m. (MonadIO m) => DecayTopIteratee a b m () 
  }

processFile :: (Iteratee Event CountIO a) -> FilePath -> IO (a,Int)
processFile iter fp = do 
  putStrLn $ "process " ++ fp   
  E.run_ $ enumFile fp $$ (ungzip =$= parseBytes def) =$ (runStateI (0::Int) iter)
  -- withFile fp ReadMode $ \ih -> runStateT (parseXmlFile ih iter) (0::Int)
                  

lheventIter :: DecayTopIteratee LHEvent PtlInfoMap CountIO c -> Iteratee Event CountIO (Int,(),c)
lheventIter action = do 
  let process = enumZip3 countIter countMarkerIter action
  header <- textLHEHeader
  parseEventIter $ decayTopEnee =$ ordDecayTopEnee =$ process



doSingleFileAnalysis :: SingleFileAnalysis -> IO ()
doSingleFileAnalysis SingleFileAnalysisDraw1DHistFromLHE{..} = 
    processFile (lheventIter (hist1dfunc hist1d)) datafile >> return ()
doSingleFileAnalysis SingleFileAnalysisCountingLHE{..} = 
    processFile (lheventIter countfunc) datafile >> return ()


-- | deprecated 

doReadXmlOnly :: FilePath -> IO ()
doReadXmlOnly fp = do 
    let process = enumZip3 countIter countMarkerIter (showSomeEvents 30) -- (hist1dfunc hist1d)  
        iter = do 
          header <- textLHEHeader
          parseEventIter $ decayTopEnee =$ process
    r <- processFile iter fp 
    putStrLn $ show r 
    return ()

