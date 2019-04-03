module CSVMaps(
  CSVMap
, keyCSV
) where

import Text.CSV
import Data.Map (Map)
import qualified Data.Map as M


type CSVMap = Map Field Record

findKey :: Monad m => Field -> Record -> m Int
findKey key hdr
  = fk key 0 hdr
  where
    fk _ _ [] = fail ("findKey: '"++key++"' not found")
    fk key i (f:fs)
      | key == f   =  return i
      | otherwise  =  fk key (i+1) fs

getKey :: Monad m => Int -> Record -> m Field
getKey _ []      =  fail "getKey: record too short"
getKey 0 (f:_)   =  return f
getKey i (_:fs)  =  getKey (i-1) fs

mkMap :: Monad m => Int -> CSV -> m CSVMap
mkMap i csv
  = mm i M.empty csv
  where
    mm _ csvm []      =  return csvm
    mm i csvm [[]]    =  return csvm
    mm i csvm [[""]]  =  return csvm
    mm i csvm (r:rs)
      = do f <- getKey i r
           let csvm' = M.insert f r csvm
           mm i csvm' rs

keyCSV :: Monad m => Field -> CSV -> m CSVMap
keyCSV _ [] = fail "keyCSV: empty CSV"
keyCSV key (hdr:body)
  = do keyIndex <- findKey key hdr
       mkMap keyIndex body

someFunc :: IO ()
someFunc = putStrLn "someFunc"
