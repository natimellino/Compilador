module Optimize ( optimize ) where

import Lang
import MonadFD4
import Eval ( semOp )
import Subst ( subst )

optimize :: MonadFD4 m => Term -> m Term
optimize t = do opt <- checkOptim
                n <- getOptimIters
                if opt then optimizeN n t
                       else return t

optimizeN :: MonadFD4 m => Int -> Term -> m Term
optimizeN 0 t = return t
optimizeN n t = do t1 <- constantFolding t
                   t2 <- constantPropagation t1
                   if t2 /= t then optimizeN (n - 1) t2
                              else return t

constantFolding :: MonadFD4 m => Term -> m Term
constantFolding (BinaryOp i op t t') = do tt  <- constantFolding t
                                          tt' <- constantFolding t'
                                          case tt of 
                                            (Const _ (CNat n)) -> do case tt' of
                                                                       (Const _ (CNat m)) -> return $ Const i $ CNat (semOp op n m) 
                                                                       _ -> return $ BinaryOp i op tt tt'
                                            _  -> return $ BinaryOp i op tt tt'
constantFolding t@(Const _ _) = return t                                               
constantFolding t@(V _ _) = return t
constantFolding (Lam i n ty t) = do tt <- constantFolding t
                                    return $ Lam i n ty tt
constantFolding (App i t t') = do tt <- constantFolding t
                                  tt' <- constantFolding t'
                                  return $ App i tt tt'
constantFolding (Print i s t) = do tt <- constantFolding t
                                   return $ Print i s tt
constantFolding (Fix i n ty n' ty' t) = do tt <- constantFolding t
                                           return $ Fix i n ty n' ty' tt
constantFolding (IfZ i c t e) = do tc <- constantFolding c
                                   tt <- constantFolding t
                                   te <- constantFolding e
                                   return $ IfZ i tc tt te            
constantFolding (Let i n ty t t') = do tt <- constantFolding t
                                       tt' <- constantFolding t'
                                       return $ Let i n ty tt tt'

constantPropagation :: MonadFD4 m => Term -> m Term  
constantPropagation (Let i x xty t u) = do tu <- constantPropagation u
                                           case t of 
                                            c@(Const _ _) -> return $ Let i x xty t (subst c tu)
                                            _ -> do tt <- constantPropagation t
                                                    return $ Let i x xty tt tu
constantPropagation t@(Const _ _) = return t
constantPropagation t@(V _ _) = return t
constantPropagation (Lam i n ty t) = do tt <- constantPropagation t
                                        return $ Lam i n ty tt
constantPropagation (App i t t') = do tt <- constantPropagation t
                                      tt' <- constantPropagation t'
                                      return $ App i tt tt'
constantPropagation (Print i s t) = do tt <- constantPropagation t
                                       return $ Print i s tt
constantPropagation (BinaryOp i op t t') = do tt  <- constantPropagation t
                                              tt' <- constantPropagation t'
                                              return $ BinaryOp i op tt tt'                                       
constantPropagation (Fix i n ty n' ty' t) = do tt <- constantPropagation t
                                               return $ Fix i n ty n' ty' tt
constantPropagation (IfZ i c t e) = do tc <- constantPropagation c
                                       tt <- constantPropagation t
                                       te <- constantPropagation e
                                       return $ IfZ i tc tt te  