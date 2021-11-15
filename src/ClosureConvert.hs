module ClosureConvert where

import Lang
import IR
import Control.Monad.Writer
import Control.Monad.State

closureConvert :: Term -> StateT Int (Writer [IrDecl]) Ir
closureConvert  (V _ nm) = case nm of
                                (Free name) -> return $ IrVar name
                                (Global name) -> return $ IrGlobal name
                                (Bound i) -> undefined
closureConvert (Const _ c) = return $ IrConst c
closureConvert (App _ t u) = do t' <- closureConvert t
                                u' <- closureConvert u
                                return $ IrCall t' [u']
closureConvert (Print _ str t) = do t' <- closureConvert t 
                                    return $ IrPrint str t'
closureConvert (BinaryOp _ op t u) = do t' <- closureConvert t 
                                        u' <- closureConvert t
                                        return $ IrBinaryOp op t' u'
closureConvert (IfZ _ c t e) = do c' <- closureConvert c
                                  t' <- closureConvert t
                                  e' <- closureConvert e
                                  return $ IrIfZ c' t' e'
closureConvert (Let _ x _ t u) = do t' <- closureConvert t
                                    u' <- closureConvert u
                                    return $ IrLet x t' u'
closureConvert _ = undefined

runCC :: [Decl Term] -> [IrDecl]
runCC xs = runWriter $ runStateT (closureConvert t) 0
           where go (decl : xs) = undefined