module Build where

-- | Build Test Suite
import Spec

main :: IO ()
main = 
  putStrLn "Test result generation" >>
  runTestWith "test/generate_result.sh "
