module Main where

import System.Environment
import Text.CSV
import CSVMaps

main :: IO ()
main
  = do  args <- getArgs
        putStrLn ("mrg "++show args)
        res <- parseCSVFromFile "test.csv"
        case res of
          Left perr -> putStrLn $ show perr
          Right csv
           -> do putStrLn $ show csv
                 csvmap <- keyCSV "KEY" csv
                 putStrLn $ show csvmap
       -- case args of
       --  (username:fpath:_) -> process username fpath
       --  _ ->  putStr (",assesslog,bad arguments"++',':show args)
