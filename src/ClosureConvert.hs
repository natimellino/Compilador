module ClosureConvert ( runCC ) where

import Lang
import IR
import Control.Monad.Writer
import Control.Monad.State
import Subst

freshName :: Name -> StateT Int (Writer [IrDecl]) Name
freshName str = do n <- get
                   put (n + 1)
                   return $ str ++ show n

closureConvert :: Term -> StateT Int (Writer [IrDecl]) Ir
closureConvert  (V _ nm) = case nm of
                                (Free name) -> return $ IrVar name
                                (Global name) -> return $ IrGlobal name
                                (Bound i) -> undefined
closureConvert (Const _ c) = return $ IrConst c
closureConvert (App _ t u) = do t' <- closureConvert t
                                case t' of
                                  (MkClosure nm env) -> do u' <- closureConvert u
                                                           return $ IrLet ("clos_" ++ nm) t' (IrCall (IrAccess t' 0) [t', u'])                      
                                  _ -> undefined
-- closureConvert (App _ t u) = do t' <- closureConvert t
--                                 u' <- closureConvert u
--                                 return $ IrLet ("clos_" ++ nm) t' (IrCall (IrAccess t' 0) [t', u'])
closureConvert (Print _ str t) = do t' <- closureConvert t 
                                    return $ IrPrint str t'
closureConvert (BinaryOp _ op t u) = do t' <- closureConvert t 
                                        u' <- closureConvert u
                                        return $ IrBinaryOp op t' u'
closureConvert (IfZ _ c t e) = do c' <- closureConvert c
                                  t' <- closureConvert t
                                  e' <- closureConvert e
                                  return $ IrIfZ c' t' e'
closureConvert (Let i x _ t u) = do t' <- closureConvert t
                                    fnm <- freshName x
                                    u' <- closureConvert (subst (V i (Free fnm)) u)
                                    return $ IrLet fnm t' u'
closureConvert (Lam _ _ _ t) = do 


runCC :: [Decl Term] -> [IrDecl]
runCC xs = snd $ runWriter $ runStateT (go xs) 0
           where go [] = return [] 
                 go ((Decl _ x _ body) : xs) = do body' <- closureConvert body
                                                  let irDecl = case body' of
                                                                 (MkClosure nm env) -> IrFun nm ["xd"] body'
                                                                 _ -> IrVal x body'
                                                  tell [irDecl]
                                                  go xs

freeVars :: Term -> [Name]
freeVars (V _ v) = case v of
                      (Bound _) -> []
                      (Free nm) -> [nm]
                      (Global _) -> []
freeVars (Const _ _) = []
freeVars (Lam _ _ _ t) = freeVars
