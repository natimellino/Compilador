module Optimize ( optimize ) where

import Lang
import MonadFD4
import Eval ( semOp )

optimize :: MonadFD4 m => Term -> m Term
optimize t = do opt <- checkOptim
                n <- getOptimIters
                if opt then optimizeN n t
                       else return t

optimizeN :: MonadFD4 m => Int -> Term -> m Term
optimizeN 0 t = return t
optimizeN n t = do t1 <- constantFolding t
                --    t2 <- deadCode t1
                   optimizeN (n - 1) t1

constantFolding :: MonadFD4 m => Term -> m Term
constantFolding ttt@(BinaryOp i op t t') = do tt  <- constantFolding t
                                              case tt of 
                                               (Const _ (CNat n)) -> do tt' <- constantFolding t'
                                                                        case tt' of
                                                                          (Const _ (CNat m)) -> return $ Const i (semOp op n m) 
                                                                          _ -> return ttt
                                               _  -> return ttt
constantFolding t = undefined