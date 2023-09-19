{-# OPTIONS_GHC -F -pgmF htfpp #-}

-- | The main test Program.
-- Original Source Code: https://github.com/skogsbaer/HTF/tree/master/sample

module Spec where

import Test.Framework
import Test.Framework.BlackBoxTest

runTestWith :: String -> IO ()
runTestWith script = do
  bbts <- blackBoxTests "test/correctos" script ".fd4" bbTArg
  htfMain ([makeTestSuite "bbts" bbts])
  where
    bbTArg = defaultBBTArgs
      { bbtArgs_stdoutSuffix = ".fd4.out"
      , bbtArgs_stderrSuffix = ".fd4.err"
      }
