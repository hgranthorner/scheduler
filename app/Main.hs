module Main where

import System.Environment (getArgs)
import Lib (executeCommand)

main :: IO ()
main = do
  args <- getArgs
  executeCommand args
  return ()
