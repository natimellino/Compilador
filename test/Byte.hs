module Byte where

-- | Bytecode Test Suite
import Spec

main :: IO ()
main = 
  putStrLn "Byte Test Suite" >>
  runTestWith "test/byte_run.sh "
