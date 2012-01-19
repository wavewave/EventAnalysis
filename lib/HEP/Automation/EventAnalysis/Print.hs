{-# LANGUAGE NoMonomorphismRestriction #-}

module HEP.Automation.EventAnalysis.Print where

import HEP.Parser.LHEParser.Type
import HEP.Parser.LHEParser.DecayTop

import Control.Monad.Trans
import Data.Enumerator 
import qualified Data.Enumerator.List as EL

dummyanalysis = const (return ()) 

showSomeEvents :: (MonadIO m) => Integer -> Iteratee (Maybe (a,b,[DecayTop PtlIDInfo])) m ()
showSomeEvents n = do 
  lst <- EL.take n 
  mapM_ (maybe (return ()) $ \(_,_,dtops) -> do mapM_ (liftIO . print . fmap pdgid ) dtops 
                                                liftIO $ putStrLn "-----------------------" ) lst 


