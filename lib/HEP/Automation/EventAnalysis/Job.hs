{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module HEP.Automation.EventAnalysis.Job where

import HROOT
import HEP.Automation.EventAnalysis.FileDriver 
import HEP.Automation.EventAnalysis.Print 

import Control.Monad
import Control.Monad.Trans

import Data.IORef

import HEP.Automation.MadGraph.SetupType
import HEP.Physics.TTBar.Analysis.TopPairParton

import qualified Data.ByteString.Lazy as L
import Data.Aeson
import qualified Data.Attoparsec.Lazy as A
import Data.Attoparsec.Char8
import Control.Applicative ((<|>))

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


import System.IO
import System.Directory 

import Data.Enumerator.Util (zipStreamWithList)
import Data.Enumerator ((=$))
import qualified Data.Enumerator.List as EL 

import HEP.Parser.LHEParser.Type 

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
                                               , countfunc = showSomeEvents 10 } 
-- afbTTBar >>= liftIO . print }
  doSingleFileAnalysis analysis 


startJunjie :: FilePath -> FilePath -> IO () 
startJunjie lhefile outfile =
    withFile outfile WriteMode $ \h -> processFile (lheventIter $  zipStreamWithList [1..]  =$ iter h ) lhefile >> hPutStrLn h "0 0 " >> return () 
  where iter h = EL.foldM (\() a -> maybe (return ()) (\(n,x)->liftIO $ printfunc h n x) a) ()  

{-
do 
          mel <- EL.head 
          -- let newlst = zip [1..] lst
          maybe (return ()) (\mx -> maybe (return ()) (\(n,x)->liftIO $ printfunc h n x) mx >> f h) mel  
          
          -- mapM_ (\(n,x)->liftIO $ (printfunc h n x)) newlst 

          {- mapM_ (maybe (return ()) $ \(n,(a,_,dtops)) -> do -- mapM_ (liftIO . print . fmap pdgid ) dtops 
                                                            liftIO . print $ (n,a) -- mapM_ (liftIO . print) dtops 
                                                            liftIO $ putStrLn "-----------------------" ) newlst  -}


--           mapM_ (maybe (return ()) $ \(a,_,dtops) -> do mapM_ (liftIO . print) a
--                                                         liftIO $ putStrLn "------------------") lst 

-}

printfunc :: Handle -> Int -> Maybe (LHEvent,a,b) -> IO () 
printfunc h n (Just (ev@(LHEvent einfo pinfos),_,_)) = do
  let EvInfo _ _ wgt _ _ _ = einfo 
  hPutStrLn h (show n ++ "  " ++ show wgt) 
  hPutStrLn h ("0 0 0 ")
  let formatter x = let (px,py,pz,pe,_) = pup x
                    in  hPutStrLn h $ show (idup x) ++ " " ++ show px ++ " " ++ show py ++ " " ++ show pz ++ " " ++ show pe ++ " 0 0 "
  mapM_ formatter pinfos 
  hPutStrLn h "0"
printfunc h n Nothing = return () 




startJsonConvert :: FilePath -> FilePath -> IO ()
startJsonConvert oldfile newfile = do 
  bstr <- L.readFile oldfile
  let pjsonresult = A.parse json bstr 
  case pjsonresult of 
    A.Done _ (Array jsonresult) -> do 
      let nvec = V.map convertJobInfo jsonresult
      let nbstr = toLazyByteString . AE.fromValue $ Array nvec
      L.writeFile newfile nbstr  


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


singleJob :: Handle -> JobInfo -> IO () 
singleJob hndl jinfo = do 
  let fnbase = getFileName jinfo 
  let fp = (map toLower fnbase) ++ "_pythia_events.lhe.gz"
      wrdir = (jobdetail_remotedir . jobinfo_detail ) jinfo
  b <- checkNdownloadFile webdav_config wrdir fp -- fetchFile webdav_config wrdir fp  
  when b $ do 
    let analysis = SingleFileAnalysisCountingLHE { datafile = fp 
                                               , countfunc = do r <- countFBTTBar 
                                                                liftIO $ hPutStrLn hndl (fnbase ++ " : " ++ show r)
                                               }
    doSingleFileAnalysis analysis 


crossSectionReadJob :: Handle -> JobInfo -> IO ()
crossSectionReadJob hndl jinfo = do 
  let fnbase = getFileName jinfo 
      fp = (map toLower fnbase) ++ "_pythia.log"
      wrdir = (jobdetail_remotedir . jobinfo_detail ) jinfo 
  fetchFile webdav_config wrdir fp 
  xsec <- getCrossSection fp 
  hPutStrLn hndl (fnbase ++ " : " ++ show xsec)
    
startMultiAnalysis :: FilePath -> IO ()
startMultiAnalysis fp = do
  hndl <- openFile "test.log" WriteMode
  let getremotedir = webdav_remotedir . jobdetail_remotedir . jobinfo_detail 
  jinfolst <- getJobInfoList fp 
  let analtypegrouped = sortNgroupBy (enumjobdetail.jobinfo_detail) jinfolst 
      eventgengroup = head analtypegrouped 
      dirgrouped = sortNgroupBy getremotedir eventgengroup
      testdirgroup = (dirgrouped !! 33 ) -- 43 40 38 36 34 33 31 29 27 
      processgrouped = sortNgroupBy getProcessBrief testdirgroup 
      testprocessgroup = head processgrouped 
      paramgrouped = sortNgroupBy getParamStr testprocessgroup
      testparamgroup = head paramgrouped 

  
  setCurrentDirectory "working"
  -- mapM_ (singleJob hndl . head) paramgrouped 
  mapM_ (crossSectionReadJob hndl . head) paramgrouped  
 

  hClose hndl 
  
  -- mapM_ (print.head) paramgrouped

  -- mapM_ ( \x -> putStrLn $ show ( fst x , (getremotedir.head.snd) x ) )  (zip [0..] dirgrouped) 

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


getCrossSection :: FilePath -> IO Double 
getCrossSection fp = do 
  bstr <- L.readFile fp 
  let r = A.parse parseCrossSection bstr 
  case r of 
    A.Done _ xsec -> return xsec
    _ -> error "parse failed"


parseCrossSection :: A.Parser Double
parseCrossSection = do 
  takeTill (== 'C') 
  (try ( string "Cross section (pb):" >> skipSpace >> double )
   <|> (char 'C' >> parseCrossSection ))