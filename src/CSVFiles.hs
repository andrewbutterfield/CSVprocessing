module CSVFiles(
  readCSV
) where

import Text.CSV

readCSV :: String -> IO CSV
readCSV file
  = do res <- parseCSVFromFile file
       case res of
         Left perr -> fail $ show perr
         Right csv -> return csv
