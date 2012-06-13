{-# LANGUAGE NoMonomorphismRestriction #-}

module HEP.Automation.EventAnalysis.Print where

import HEP.Parser.LHEParser.Type
import HEP.Parser.LHEParser.DecayTop

import Control.Monad.Trans
import Data.Conduit
import qualified Data.Conduit.List as CL

dummyanalysis = const (return ()) 

showSomeEvents :: (MonadIO m) => Int -> Sink (Maybe (a,b,[DecayTop PtlIDInfo])) m ()
showSomeEvents n = do 
  lst <-CL.take n 
  mapM_ (maybe (return ()) $ \(_,_,dtops) -> do -- mapM_ (liftIO . print . fmap pdgid ) dtops 
                                                mapM_ (liftIO . print) dtops 
                                                liftIO $ putStrLn "-----------------------" ) lst 


