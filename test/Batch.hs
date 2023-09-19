-- | Batch option

module Batch where

import Spec

main :: IO ()
main = 
  putStrLn "Batch execution test suite" >>
  putStrLn "========================================" >>
  runTestWith "test/batch_run.sh "
