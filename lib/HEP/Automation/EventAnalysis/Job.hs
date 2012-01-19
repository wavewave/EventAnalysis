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
import HEP.Automation.JobQueue.JobType
import HEP.Automation.JobQueue.JobJson
import HEP.Automation.MadGraph.Util 

import qualified Data.Text as T
-- import Data.Text hiding (take, groupBy,length, null, fitler)
import Data.Either 
-- import Data.Text.Lazy.Builder
-- import Data.Text.Lazy.Encoding
import Blaze.ByteString.Builder 
import Data.Aeson.Encode as AE

import Data.Function
import Data.List 
import Data.Char
import HEP.Storage.WebDAV 
import HEP.Storage.WebDAV.Type



import System.Directory 

webdav_config  :: WebDAVConfig 
webdav_config = WebDAVConfig { webdav_path_wget = "/usr/local/bin/wget"
                             , webdav_path_cadaver = "/Users/iankim/opt/homebrew/bin/cadaver"
                             , webdav_baseurl = "http://top.physics.lsa.umich.edu:10080/webdav/montecarlo" } 

startSingle :: FilePath -> FilePath -> IO () 
startSingle lhefile pdffile = do 
  putStrLn "job started"
  putStrLn $ lhefile 
  putStrLn $ pdffile 
  
  h1 <- newTH1D "test" "test" 50 (-1.2) 1.2 

  let analysis = SingleFileAnalysisCountingLHE { datafile = lhefile
                                               , countfunc = afbTTBar >>= liftIO . print }
  doSingleFileAnalysis analysis 

startJsonConvert :: FilePath -> FilePath -> IO ()
startJsonConvert oldfile newfile = do 
  bstr <- L.readFile oldfile -- "jobqueueserver20110909.json"  
  let pjsonresult = A.parse json bstr -- decode bstr 
  case pjsonresult of 
    A.Done _ (Array jsonresult) -> do 
      let nvec = V.map convertJobInfo jsonresult
      let nbstr = toLazyByteString . AE.fromValue $ Array nvec
      L.writeFile newfile nbstr  -- "jobqueueserver20110909_convert20120119.json" nbstr


startJsonTest :: FilePath -> IO () 
startJsonTest fp = do 
  bstr <- L.readFile fp 
  let pjsonresult = A.parse json bstr
  case pjsonresult of 
    A.Done _ jsonresult -> do 
      let eresult :: Either String [JobInfo] = fromAeson jsonresult
      case eresult of
        Left str -> putStrLn str 
        Right lst -> do 
          print (Prelude.length lst)
          print (Prelude.head lst)


getJobInfoList :: FilePath -> IO [JobInfo]
getJobInfoList fp = do
  bstr <- L.readFile fp 
  let pjsonresult = A.parse json bstr
  case pjsonresult of 
    A.Done _ jsonresult -> do 
      let eresult :: Either String [JobInfo] = fromAeson jsonresult
      case eresult of
        Left str -> error str 
        Right lst -> return lst 

enumjobdetail :: JobDetail -> (Int,String)
enumjobdetail (EventGen _ _) = (0,"eventgen")
enumjobdetail (MathAnal str _ _ ) = (1,str) 


getProcessBrief :: JobInfo -> String 
getProcessBrief jinfo = let evset = jobdetail_evset . jobinfo_detail $ jinfo 
                        in case evset of 
                             EventSet psetup rsetup -> processBrief psetup

getParamStr :: JobInfo -> String
getParamStr jinfo =  let evset = jobdetail_evset . jobinfo_detail $ jinfo 
                     in case evset of 
                          EventSet psetup rsetup -> show $ param rsetup

getSetNum :: JobInfo -> Int
getSetNum jinfo = let evset = jobdetail_evset . jobinfo_detail $ jinfo 
                  in case evset of 
                       EventSet psetup rsetup -> setnum rsetup

getFileName :: JobInfo -> FilePath 
getFileName jinfo = let evset = jobdetail_evset . jobinfo_detail $ jinfo 
                    in case evset of 
                         EventSet psetup rsetup -> makeRunName psetup rsetup

sortNgroupBy :: (Ord b, Eq b) => (a -> b) -> [a] -> [[a]]
sortNgroupBy accessor = groupBy ((==) `on` accessor) . sortBy (compare `on` accessor) 

startMultiAnalysis :: FilePath -> IO ()
startMultiAnalysis fp = do
  let getremotedir = webdav_remotedir . jobdetail_remotedir . jobinfo_detail 
  jinfolst <- getJobInfoList fp 
  let analtypegrouped = sortNgroupBy (enumjobdetail.jobinfo_detail) jinfolst 
  

      eventgengroup = head analtypegrouped 
      dirgrouped = sortNgroupBy getremotedir eventgengroup
      testdirgroup = (dirgrouped !! 4)
      processgrouped = sortNgroupBy getProcessBrief testdirgroup 
      testprocessgroup = head processgrouped 
      paramgrouped = sortNgroupBy getParamStr testprocessgroup
      testparamgroup = head paramgrouped 

      testinfo = head testparamgroup 
  
  let fp = (map toLower (getFileName testinfo) ) ++ "_pythia_events.lhe.gz"
      wrdir = (jobdetail_remotedir . jobinfo_detail ) testinfo

  setCurrentDirectory "working"
  fetchFile webdav_config wrdir fp  

  let analysis = SingleFileAnalysisCountingLHE { datafile = fp 
                                               , countfunc = countFBTTBar >>= liftIO . print } 

  doSingleFileAnalysis analysis 

  -- mapM_ (print . getFileName ) testparamgroup
  -- mapM_ (print . getSetNum )  (head paramgrouped )



checkRdir :: T.Text -> M.HashMap T.Text Value -> Bool 
checkRdir rdir val = let Just (String txt) = M.lookup "rdir" val
                     in  (txt == rdir)


getOneDetail :: Value -> Value 
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
   let Just (Object evset) = M.lookup "evset" item
       Just (Object rsetup) = M.lookup "rsetup" evset
       nrsetup = M.insert "lhesanitizer" (String "NoLHESanitize") rsetup 
       nevset = M.adjust (const (Object nrsetup)) "rsetup" evset
       nitem = M.adjust (const (Object nevset)) "evset" item 
   in  (Object nitem)
convertOneDetail _ = error "cannot deal with it" 
