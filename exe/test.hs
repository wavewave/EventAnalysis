{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

-- module HEP.Automation.EventAnalysis.Job where

--import HROOT

import HEP.Parser.LHEParser.DecayTop
import HEP.Automation.EventAnalysis.FileDriver 
import HEP.Automation.EventAnalysis.Print 
import HEP.Util.Functions

import Data.List.Split 
import Data.Vector.Storable ((!))

import Debug.Trace 
import Control.Applicative 
import Control.Monad
import Control.Monad.Trans

import Text.Printf
import qualified  Numeric.LinearAlgebra as NL

import Data.Either 
import Data.Function
import Data.List 
import Data.Char

import System.FilePath

import System.IO
import System.Directory 

import Data.Enumerator.Util (zipStreamWithList)
import Data.Enumerator ((=$))
import qualified Data.Enumerator.List as EL 

import HEP.Parser.LHEParser.Type 

-- | 

zipWithM3 :: (Monad m) => (a -> b -> c -> m d) -> [a] -> [b] -> [c] -> m [d]
zipWithM3 f xs ys zs = sequence (zipWith3 f xs ys zs)

-- | 

main :: IO () 
main = do 
  startMine "test.lhe.gz" "test2.lhe.gz" "test3.lhe.gz"
            "testoutput.dat"

-- | 

startJunjie :: FilePath -> FilePath -> IO () 
startJunjie lhefile outfile =
    withFile outfile WriteMode $ \h -> processFile (lheventIter $  zipStreamWithList [1..]  =$ iter h ) lhefile >> hPutStrLn h "0 0 " >> return () 
  where iter h = EL.foldM (\() a -> maybe (return ()) (\(n,x)->liftIO $ printfunc h n x) a) ()  

-- | 

startMine :: FilePath  -- ^ main hard process
          -> FilePath  -- ^ decay process1
          -> FilePath  -- ^ decay process2
          -> FilePath  -- ^ output 
          -> IO () 
startMine lhefile lhefile2 lhefile3 outfile =
    withFile outfile WriteMode $ 
    -- flip ($) stdout $
      \h -> do 
        ((_,_,lst1),_) <- processFile 
                            (lheventIter $  zipStreamWithList [1..]  =$ iter h ) 
                            lhefile 
        ((_,_,lst2),_) <- processFile 
                            (lheventIter $  zipStreamWithList [1..]  =$ iter h ) 
                            lhefile2 
        ((_,_,lst3),_) <- processFile 
                            (lheventIter $ zipStreamWithList [1..] =$ iter h )
                            lhefile3 
        zipWithM3 (iw h) lst1 lst2 lst3
        hPutStrLn h $ "lst1 = " ++ show (length lst1)
        hPutStrLn h $ "lst2 = " ++ show (length lst2)
        hPutStrLn h $ "lst3 = " ++ show (length lst3)
  where iw h (Just (_,a)) (Just (_,b)) (Just (_,c)) = interwine3 h a b c
        iw _ _ _ _ = return () 
        iter h = EL.foldM (\lst a -> maybe (return lst) 
                                       (\(n,x) -> return (a:lst) ) a) 
                          []

-- | 

getN1 :: [PtlInfo] -> ([PtlInfo],[PtlInfo])
getN1 = break (\x -> idup x == 1000022 && istup x == 1)    

-- | 

adjustFirst :: Int -> [PtlInfo] -> [PtlInfo]
adjustFirst n (x:xs) = x { mothup = (n,n), istup = 2 } : xs 

-- | 

boostBack :: FourMomentum -> PtlInfo -> PtlInfo 
boostBack mom pinfo = 
  let v3 = NL.scale (-1) .  beta . fourMomentumToLorentzVector $ mom
      lrot = boost v3  
      v = lrot NL.<> fourMomentumToLorentzVector (pupTo4mom (pup pinfo))
      e = v ! 0
      px = v ! 1 
      py = v ! 2 
      pz = v ! 3 
      masssqr = e*e-px*px-py*py-pz*pz
      mass = if masssqr < 0 then 0 else if masssqr < 1e-3 then 0 else sqrt masssqr
      npup = (px,py,pz,e,mass)
  in pinfo { pup = npup }
      
-- | 

spinAdj :: Double -> PtlInfo -> PtlInfo 
spinAdj spn pinfo = pinfo { spinup = spn * spinup pinfo } 

-- | 

idAdj :: (Int -> Int) -> PtlInfo -> PtlInfo 
idAdj idfunc pinfo = pinfo { mothup = (idfunc mid1, idfunc mid1 {- mid2 -} ) } 
                     -- because of bug in madgraph 
  where (mid1,_mid2) = mothup pinfo

-- | 

adjustIdMomSpin :: (PtlInfo,PtlInfo)-> [PtlInfo] -> [PtlInfo]
adjustIdMomSpin (opinfo,rpinfo) = map (idAdj idfunc . spinAdj spn . boostBack mom) 
  where mom = pupTo4mom . pup $ opinfo 
        spn = spinup opinfo * spinup rpinfo 
        ido = ptlid opinfo 
        idr = ptlid rpinfo 
        idfunc x = if x == idr then ido else x

-- | 

interwine3 :: Handle -> Maybe (LHEvent,a,b) -> Maybe (LHEvent,a,b) -> Maybe (LHEvent,a,b)
              -> IO ()
interwine3 h (Just (lhe1,_,_)) 
             (Just (lhe2,_,_)) 
             (Just (lhe3,_,_)) = do 
  let lhe1' = interwine2 lhe1 lhe2
      lhe1_final = interwine2 lhe1' lhe3
  hPutStrLn h (lheFormatOutput lhe1_final)

-- | 

interwine2 :: LHEvent -> LHEvent -> LHEvent 
interwine2 (LHEvent einfo1 pinfos1) (LHEvent einfo2 pinfos2) =  
  let ptlids1 = map ptlid pinfos1
      icols1 = filter (/= 0) (concatMap ((\x -> [fst x, snd x]) . icolup )
                                pinfos1)
      maxid1 = maximum ptlids1 
      maxicol1 = maximum icols1 
      minicol1 = minimum icols1 
      npinfos2'  = map (adjustIds (idChange (maxid1-1)) (colChange (maxicol1-minicol1+1)))
                       pinfos2
      (first1,(n1:rest1)) = getN1 pinfos1
      pinfos1' = first1 ++ (n1 { istup = 2} : rest1)
      rn1  = head $ npinfos2' 
      (_:npinfos2) = adjustIdMomSpin (n1,rn1) . adjustFirst (ptlid n1) $ npinfos2'  
      npinfos = pinfos1' ++ npinfos2
      numptls = length npinfos
      neinfo = einfo1 { nup = numptls }
  in LHEvent neinfo npinfos 

  


-- |

endl = "\n"

-- |

lheFormatOutput :: LHEvent -> String 
lheFormatOutput (LHEvent einfo pinfos) =
  "<event>" ++ endl 
  ++ printf "%2d" (nup einfo) -- ++ "  " 
  ++ printf "%4d" (idprup einfo) ++ " " 
  ++ printf "%14.7E" (xwgtup einfo) ++ " " 
  ++ printf "%14.7E" (scalup einfo) ++ " " 
  ++ printf "%14.7E" (aqedup einfo) ++ " " 
  ++ printf "%14.7E" (aqcdup einfo) ++ endl 
  ++ concatMap pformat pinfos 
  ++ "</event>" -- ++ endl

-- | 

pformat :: PtlInfo -> String 
pformat pinfo = 
    printf "%9d" (idup  pinfo)
    ++ printf "%5d" (istup pinfo)
    ++ printf "%5d" (fst (mothup pinfo))
    ++ printf "%5d" (snd (mothup pinfo))
    ++ printf "%5d" (fst (icolup pinfo))
    ++ printf "%5d" (snd (icolup pinfo))
    ++ printf "%19.11E" pupx
    ++ printf "%19.11E" pupy
    ++ printf "%19.11E" pupz
    ++ printf "%19.11E" pupt 
    ++ printf "%19.11E" pupm 
    ++ printf "%4.1f" (vtimup pinfo)
    ++ printf "%5.1f" (spinup pinfo)
    ++ endl 
  where (pupx,pupy,pupz,pupt,pupm) = pup pinfo 


-- |

printfunc :: Handle -> Int -> Maybe (LHEvent,a,b) -> IO () 
printfunc h n (Just (ev@(LHEvent einfo pinfos),_,_)) = do
  let EvInfo _ _ wgt _ _ _ = einfo 
      ptlids = map ptlid pinfos
      icols = filter (/= 0) (concatMap ((\x -> [fst x, snd x]) . icolup )
                               pinfos)
      maxid = maximum ptlids 
      maxicol = maximum icols 
      minicol = minimum icols 
      npinfos  = map (adjustIds (idChange maxid)
                                (colChange (maxicol-minicol))
                     ) 
                     pinfos 
  hPutStrLn h (show n ++ "  " ++ show wgt) 
  hPutStrLn h ("0 0 0 ")
  hPutStrLn h (show (mkIntTree pinfos))
  hPutStrLn h (show ptlids)
  hPutStrLn h (show maxid)
  hPutStrLn h (show icols) 
  hPutStrLn h (show maxicol)
  hPutStrLn h "*********************"
  hPutStrLn h (show pinfos)
  hPutStrLn h "---------------------" 
  hPutStrLn h (show npinfos)
  hPutStrLn h "====================="
{-
  let formatter x = let (px,py,pz,pe,_) = pup x
                    in  hPutStrLn h $ show (idup x) ++ " " ++ show px ++ " " ++ show py ++ " " ++ show pz ++ " " ++ show pe ++ " 0 0 "
  mapM_ formatter pinfos 
  hPutStrLn h "0" -}
printfunc h n Nothing = return () 

-- | 

idChange :: Int -> Int -> Int
idChange r a = a + r

-- | 

colChange :: Int -> Int -> Int 
colChange r 0 = 0 
colChange r a = a + r 

-- | 

adjustIds :: (Int -> Int) -> (Int->Int) -> PtlInfo -> PtlInfo
adjustIds idmap icolmap pinfo = 
    pinfo { ptlid = idmap optlid
          , mothup = (idmap omothup_x, idmap omothup_y) 
          , icolup = (icolmap oicolup_x, icolmap oicolup_y)
          } 
  where optlid = ptlid pinfo 
        (omothup_x, omothup_y) = mothup pinfo
        (oicolup_x, oicolup_y) = icolup pinfo 
         