module Main where

import System.Environment
import Text.CSV
import Data.Set (Set)
import qualified Data.Set as S
import Data.Map (Map)
import qualified Data.Map as M

import CSVMaps
import CSVFiles

main :: IO ()
main
 = do args <- getArgs
      putStrLn ("mrg "++show args)
      (file1,key1,file2,key2) <- processArgs args
      putStrLn (unwords [file1,key1,file2,key2])
      csv1 <- readCSV file1
      m1@(hdr1,csvmap1) <- keyCSV key1 csv1
      putStrLn ("CSVMap1:\n" ++ show csvmap1)
      csv2 <- readCSV file2
      m2@(hdr2,csvmap2) <- keyCSV key2 csv2
      putStrLn ("CSVMap2:\n" ++ show csvmap2)
      outmap <- merge m1 m2
      putStrLn ("Merge:\n" ++ show outmap)
      let outCSV = (hdr1++hdr2) : unKeyMap outmap
      putStrLn $ printCSV outCSV

processArgs ["-h"]                   =  help
processArgs ["--help"]               =  help
processArgs [file1,file2,key]        =  checkArgs file1 key file2 key
processArgs [file1,file2,key1,key2]  =  checkArgs file1 key1 file2 key2
processArgs _                        =  help

help
 = do putStr $ unlines
       [ "usage: mrgcsv csvfile1 csvfile2 key [key2]"
       , "       mrgcsv [-h] [--help]"
       ]
      fail "no files/keys to merge"

checkArgs file1 key1 file2 key2
 = do file1OK <- forceCSV file1
      file2OK <- forceCSV file2
      return (file1OK,key1,file2OK,key2)

forceCSV filename
 = if take 4 (reverse filename) == "vsc."
   then return filename
   else if '.' `elem` filename
        then fail ("file '"++filename++"' has non .csv extension")
        else return (filename++".csv")

merge :: Monad m => (Record,CSVMap) -> (Record,CSVMap) -> m CSVMap
merge (hdr1,map1) (hdr2,map2)
 = do let keys1 = M.keysSet map1
      let keys2 = M.keysSet map2
      let (map1ovl,map1dsj)
            = M.partitionWithKey (\k _ -> k `S.member` keys2) map1
      let (map2ovl,map2dsj)
            = M.partitionWithKey (\k _ -> k `S.member` keys1) map2
      let post1 = blank (length hdr2)
      let pre2 = blank (length hdr1)
      let mapovl = M.unionWith (++) map1ovl map2ovl
      let map1ext = M.map (++post1) map1dsj
      let map2ext = M.map (pre2++) map2dsj
      return $ M.unions [map1ext,mapovl,map2ext]

blank :: Int -> Record
blank i = replicate i ""
