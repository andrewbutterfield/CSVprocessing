module Main where

import System.Environment
import Text.CSV
import CSVMaps

main :: IO ()
main
  = do  args <- getArgs
        putStrLn ("upd "++show args++" N.Y.I.")
