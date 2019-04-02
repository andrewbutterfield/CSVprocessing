module Main where

import Text.CSV
import CSVFiles

main :: IO ()
main = do putStrLn "mrg!"
          res <- parseCSVFromFile "test.csv"
          case res of
            Left perr -> putStrLn $ show perr
            Right csv
             -> do putStrLn $ show csv
                   csvmap <- keyCSV "KEY" csv
                   putStrLn $ show csvmap
