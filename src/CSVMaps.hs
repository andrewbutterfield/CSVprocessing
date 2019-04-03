module CSVMaps(
  CSVMap
, keyCSV
, unKeyMap
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

mkMap :: Monad m => Record -> Int -> CSV -> m (Record,CSVMap)
mkMap hdr i csv
  = mm M.empty csv
  where
    mm csvm []      =  return (hdr,csvm)
    mm csvm [[]]    =  return (hdr,csvm)
    mm csvm [[""]]  =  return (hdr,csvm)
    mm csvm (r:rs)
      = do f <- getKey i r
           let csvm' = M.insert f r csvm
           mm csvm' rs

keyCSV :: Monad m => Field -> CSV -> m (Record,CSVMap)
keyCSV _ [] = fail "keyCSV: empty CSV"
keyCSV key (hdr:body)
  = do keyIndex <- findKey key hdr
       mkMap hdr keyIndex body

unKeyMap :: CSVMap -> CSV
unKeyMap = M.elems
