module Main where

import System.Environment
import Text.CSV
import CSVMaps
import CSVFiles

main :: IO ()
main
 = do args <- getArgs
      putStrLn ("mrg "++show args)
      (file1,key1,file2,key2) <- processArgs args
      putStrLn (unwords [file1,key1,file2,key2])
      csv1 <- readCSV file1
      csvmap1 <- keyCSV key1 csv1
      putStrLn ("CSVMap1:\n" ++ show csvmap1)
      csv2 <- readCSV file2
      csvmap2 <- keyCSV key2 csv2
      putStrLn ("CSVMap2:\n" ++ show csvmap2)

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
