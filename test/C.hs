-- | C compilation test suite

module C where

import Spec

main :: IO ()
main =
    putStrLn "C Backend Test Suite" >>
    runTestWith "test/compile_and_run_c.sh "
