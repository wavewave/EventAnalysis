{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module HEP.Automation.EventAnalysis.Job where

import HROOT
import HEP.Automation.EventAnalysis.FileDriver 
import HEP.Automation.EventAnalysis.Print 

import Control.Monad.Trans

import Data.IORef

import HEP.Automation.MadGraph.SetupType
import HEP.Physics.TTBar.Analysis.TopPairParton

import qualified Data.ByteString.Lazy as L
import Data.Aeson
import qualified Data.Attoparsec.Lazy as A
import HEP.Automation.JobQueue.JobQueue
import qualified Data.IntMap as IM
import qualified Data.HashMap.Strict as M
import qualified Data.Vector as V
import HEP.Automation.JobQueue.JobJson

import Data.Text
import Data.Either 
-- import Data.Text.Lazy.Builder
-- import Data.Text.Lazy.Encoding
import Blaze.ByteString.Builder 
import Data.Aeson.Encode as AE


startSingle :: FilePath -> FilePath -> IO () 
startSingle lhefile pdffile = do 
  putStrLn "job started"
  putStrLn $ lhefile 
  putStrLn $ pdffile 
  
  h1 <- newTH1D "test" "test" 50 (-1.2) 1.2 

  let analysis = SingleFileAnalysisCountingLHE { datafile = lhefile
                                               , countfunc = afbTTBar >>= liftIO . print }
  doSingleFileAnalysis analysis 

startJsonTest :: IO ()
startJsonTest = do 
  putStrLn "hey"
  bstr <- L.readFile "jobqueueserver20110909.json"  
  -- let mmymap = decode bstr :: Maybe (M.IntMap JobInfo)
  -- maybe (putStrLn "fail") (const (putStrLn "success")) mmymap 
  let pjsonresult = A.parse json bstr -- decode bstr 
  case pjsonresult of 
    A.Done _ (Array jsonresult) -> do 
      let nvec = V.map convertJobInfo jsonresult
      -- let details = V.map getOneDetail jsonresult 
      -- print $ V.length details 
      -- let testitem = V.head details
      --    nitem = convertOneDetail testitem 
      -- print $ V.head nvec
      -- let (t :: Either String JobInfo) = fromAeson (V.head nvec)
      -- print t 

      -- print $ (toAeson ([] :: [Int]))
      {- let rs = rights . V.toList . V.map (fromAeson :: Value -> Either String JobInfo) $ nvec 
      print $ Prelude.length rs
      print $ V.length nvec  -}

      let nbstr = toLazyByteString . AE.fromValue $ Array nvec

      L.writeFile "jobqueueserver20110909_convert20120119.json" nbstr

      -- let (t :: Either String JobDetail) = fromAeson $ Object (V.head filtered)
      -- print t 
  --     print $ V.length filtered 
  -- putStrLn $ take 200 $ show jsonresult 


checkRdir :: Text -> M.HashMap Text Value -> Bool 
checkRdir rdir val = let Just (String txt) = M.lookup "rdir" val
                     in  (txt == rdir)


getOneDetail :: Value -> Value -- M.HashMap Text Value 
getOneDetail onevalue = 
      let Object oneitem = onevalue
          Just v@(Object detail) = M.lookup "detail" oneitem 
      in  v


convertJobInfo :: Value -> Value 
convertJobInfo (Object info) = 
  let Just d = M.lookup "detail" info 
      ninfo = M.insert "dependency" (Array V.empty)
              . M.adjust (const (convertOneDetail d)) "detail" 
              $ info
      
  in  (Object ninfo)

convertOneDetail :: Value -> Value
convertOneDetail (Object item) = 
   let -- item = V.head details 
       -- filtered = V.filter (checkRdir "paper3/ttbar_LHC_c1v_scan") details 
       Just (Object evset) = M.lookup "evset" item
       Just (Object rsetup) = M.lookup "rsetup" evset
       nrsetup = M.insert "lhesanitizer" (String "NoLHESanitize") rsetup 
       nevset = M.adjust (const (Object nrsetup)) "rsetup" evset
       nitem = M.adjust (const (Object nevset)) "evset" item 
   in  (Object nitem)
convertOneDetail _ = error "cannot deal with it" 

   -- print $ M.lookup "JobType" (V.head filtered)
   -- print evset 
   --  print rsetup 
   --    print (toAeson NoLHESanitize)
   --   print nrsetup
   --   print nevset 
