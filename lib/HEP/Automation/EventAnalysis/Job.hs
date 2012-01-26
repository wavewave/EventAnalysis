{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module HEP.Automation.EventAnalysis.Job where

import HROOT
import HEP.Automation.EventAnalysis.FileDriver 
import HEP.Automation.EventAnalysis.Print 

import Control.Applicative 
import Control.Monad
import Control.Monad.Trans

import Data.IORef

import HEP.Automation.MadGraph.SetupType
import HEP.Physics.TTBar.Analysis.TopPairParton

import qualified Data.ByteString.Lazy as L
import Data.Aeson
import qualified Data.Attoparsec.Lazy as A
import qualified Data.Attoparsec.Char8 as C
import qualified Data.ByteString as S 
import qualified Data.Attoparsec as AS
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

import Data.Mathematica
import Data.Mathematica.Parser

import System.FilePath

import System.IO
import System.Directory 

import Data.Enumerator.Util (zipStreamWithList)
import Data.Enumerator ((=$))
import qualified Data.Enumerator.List as EL 

import HEP.Parser.LHEParser.Type 


import HEP.Util.GHC.Plugins
import Unsafe.Coerce


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


{-

startJsonConvert :: FilePath -> FilePath -> IO ()
startJsonConvert oldfile newfile = do 
  bstr <- L.readFile oldfile
  let pjsonresult = A.parse json bstr 
  case pjsonresult of 
    A.Done _ (Array jsonresult) -> do 
      let nvec = V.map convertJobInfo jsonresult
      let nbstr = . toByteString . AE.fromValue $ Array nvec
      L.writeFile newfile nbstr  
-}


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
                                               , countfunc = do r <- countFBHLTTBar 
                                                                liftIO $ hPutStrLn hndl (fnbase ++ " : high = " ++ show (high r) ++ " , low = " ++ show (low r) )
                                               }
    doSingleFileAnalysis analysis 


crossSectionReadJob :: Handle -> JobInfo -> IO ()
crossSectionReadJob hndl jinfo = do 
  let fnbase = getFileName jinfo 
      fp = (map toLower fnbase) ++ "_pythia.log"
      wrdir = (jobdetail_remotedir . jobinfo_detail ) jinfo 
  b <- checkNdownloadFile webdav_config wrdir fp  -- fetchFile webdav_config wrdir fp 
  when b $ do 
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
      testdirgroup = (dirgrouped !! 43 ) -- 43 40 38 36 34 33 31 29 27 
      processgrouped = sortNgroupBy getProcessBrief testdirgroup 
      testprocessgroup = head processgrouped 
      paramgrouped = sortNgroupBy getParamStr testprocessgroup
      testparamgroup = head paramgrouped 

  
  setCurrentDirectory "working"
  mapM_ (singleJob hndl . head) paramgrouped 
  -- mapM_ (crossSectionReadJob hndl . head) paramgrouped  
  -- mapM_ (binnedPassedEvtsJob hndl . head) paramgrouped

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

readMathematicaData :: Handle -> FilePath -> IO () 
readMathematicaData h fp = do 
  putStrLn $ "reading " ++ fp 
  bstr <- S.readFile fp
  let r = AS.parseOnly mathdatafile bstr
  either print  (binpassevtformatter h) r 

binpassevtformatter :: Handle -> [[MExpression]] -> IO ()
binpassevtformatter h (l1:l2:l3:l4:[]) = do 
  let y1:y2:[] = l1
      [y3] = l2 
      y4:y5:y6:[] = l3 
      -- y7:y8:[] = l4 
      MReal sigma_tt = y1 
      MInteger num_evt = y2 
      MInteger num_passevt = y3 
      MExp _ ll1 = y4 
      -- MExp _ ll2 = y5
      MExp _ ll3 = y6
      [ bak, for ] = ll3 
      MExp _ [ MInteger lowb, MInteger highb ] = bak 
      MExp _ [ MInteger lowf, MInteger highf ] = for 
  putStrLn $ "sigma_tt = " ++ show sigma_tt
  putStrLn $ "num_evt = " ++ show num_evt
  putStrLn $ "num_passevt = " ++ show num_passevt 
  -- putStrLn $ "ll2 = " ++ show ll2
  putStrLn $ "lowf = " ++ show lowf
  putStrLn $ "lowb = " ++ show lowb 
  putStrLn $ "highf = " ++ show highf 
  putStrLn $ "highb = " ++ show highb 
  
  hPutStrLn h $ "sigma_tt = " ++ show sigma_tt ++ " , " 
                ++ "num_evt = " ++ show num_evt ++ " , " 
                ++ "num_passevt = " ++ show num_passevt ++ " , " 
                ++ "(lowf,lowb) = " ++ show (lowf,lowb) ++ " , " 
                ++ "(highf,highb) = " ++ show (highf,highb)
  

binnedPassedEvtsJob :: Handle -> JobInfo -> IO () 
binnedPassedEvtsJob hndl jinfo = do 
  let fnbase = getFileName jinfo 
  let fp = (map toLower fnbase) ++ "_binned_passedevts.dat"
      wrdir = (jobdetail_remotedir . jobinfo_detail ) jinfo 
  b <- checkNdownloadFile webdav_config wrdir fp 
  when b $ do 
    hPutStr hndl ( fnbase ++ " : " )  
    readMathematicaData hndl fp 


startLowMassAnalysis :: String -> IO () 
startLowMassAnalysis mname = do 
  putStrLn $ " compiling " ++ mname 
  let fullmname = "HEP.Automation.MadGraph.Dataset." ++ mname 
      datasetdir = "/Users/iankim/mac/data/madgraph-auto-dataset"
  (>>=) (pluginCompile datasetdir fullmname "(eventsets,webdavdir)") $ 
    either error $ \value -> do 
      let (eventsets,webdavdir) = unsafeCoerce value :: ([EventSet],WebDAVRemoteDir)
          remotepath = "/Users/iankim/mac/workspace/teststorage" </> webdav_remotedir webdavdir 
      withFile "testlow.log" WriteMode $ \h -> 
        mapM_ (startEventSetXSec h remotepath) eventsets 
        -- mapM_ (startEventSetAFBHL h remotepath) eventsets -- (Prelude.take 1 eventsets )

startEventSetXSec :: Handle -> FilePath -> EventSet -> IO ()
startEventSetXSec h fp evset = do 
  cdir <- getCurrentDirectory 
  case evset of 
    EventSet psetup rsetup -> do 
      let rname = makeRunName psetup rsetup 
          nfilename = rname ++ "_banner.txt"
      setCurrentDirectory "working"
      putStrLn "copying banner"
      copyFile (fp </> nfilename) nfilename
      bstr <- L.readFile nfilename 
      let pxsec = A.parse xsecFromBanner bstr 
      case pxsec of 
        A.Done _ xsec -> do 
          hPutStrLn h ( rname ++ " : " ++ show xsec )  
      setCurrentDirectory cdir


startEventSetAFB :: Handle -> FilePath -> EventSet -> IO ()
startEventSetAFB h fp evset = do 
  cdir <- getCurrentDirectory 
  case evset of 
    EventSet psetup rsetup -> do 
      let rname = makeRunName psetup rsetup 
          nfilename = rname ++ "_unweighted_events.lhe.gz"
      setCurrentDirectory "working"
      copyFile (fp </> nfilename) nfilename
      let analysis = SingleFileAnalysisCountingLHE { datafile = nfilename  
                                                   , countfunc = do r <- countFBTTBar 
                                                                    liftIO $ hPutStrLn h (rname ++ " : " ++ show r)
                                                   }
      doSingleFileAnalysis analysis 
      setCurrentDirectory cdir 

startEventSetAFBHL :: Handle -> FilePath -> EventSet -> IO ()
startEventSetAFBHL h fp evset = do 
  cdir <- getCurrentDirectory 
  case evset of 
    EventSet psetup rsetup -> do 
      let rname = makeRunName psetup rsetup 
          nfilename = rname ++ "_unweighted_events.lhe.gz"
      setCurrentDirectory "working"
      copyFile (fp </> nfilename) nfilename
      let analysis = SingleFileAnalysisCountingLHE { datafile = nfilename  
                                                   , countfunc = do r <- countFBHLTTBar 
                                                                    liftIO $ hPutStrLn h (rname ++ " : high = " ++ show (high r) ++ " , low = " ++ show (low r) )
                                                   }
      doSingleFileAnalysis analysis 
      setCurrentDirectory cdir 


xsecFromBanner :: Parser Double 
xsecFromBanner = do 
  takeTill (== '#') 
  (try (string "#  Integrated weight (pb)  :" *> skipSpace *> (many (C.satisfy (C.inClass ".0123456789E+-")) >>= return . readDouble . ('0':)) )
   <|> ( char '#' *> xsecFromBanner ) )


       