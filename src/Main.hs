module Main where

import           Property
import           Helper

main :: IO ()
main = do
  args <- getArgs
  when (length args /= 2) $ die "useage example: ./setpropex ro.debuggable 1"
  let [name, value] = args
  writeProperty name value
