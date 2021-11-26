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
closureConvert (Print _ str t) = do t' <- closureConvert t 
                                    nm <- freshName "x"
                                    return $ IrLet nm t' (IrPrint str (IrVar nm))
closureConvert (BinaryOp _ op t u) = do t' <- closureConvert t 
                                        u' <- closureConvert u
                                        return $ IrBinaryOp op t' u'
closureConvert (IfZ _ c t e) = do c' <- closureConvert c
                                  t' <- closureConvert t
                                  e' <- closureConvert e
                                  return $ IrIfZ c' t' e'
closureConvert (Let i x _ t u) = do t' <- closureConvert t
                                    fnm <- freshName x
                                    u' <- closureConvert (open fnm u)
                                    return $ IrLet fnm t' u'
closureConvert (App _ t u) = do t' <- closureConvert t
                                u' <- closureConvert u
                                nm <- freshName "clos_"
                                return $ IrLet nm t' (IrCall (IrAccess (IrVar nm) 0) [t', u'])                                   
closureConvert (Lam _ x _ t) = do xnm <- freshName x
                                  t' <- closureConvert (open xnm t)
                                  fnm <- freshName "g"
                                  let fv = freeVars t
                                      clnm = fnm ++ "_clos"
                                      ff = letter fv t' clnm
                                      codef = IrFun fnm [clnm, xnm] ff
                                  tell [codef]
                                  return $ MkClosure fnm (map IrVar fv)
closureConvert (Fix _ f _ x _ t) = do xnm <- freshName x
                                      fnm <- freshName $ "rec_" ++ f
                                      let clnm = fnm ++ "_clos"
                                      t' <- closureConvert (openN [clnm, xnm] t)
                                      let fv = freeVars t
                                          ff = letter fv t' clnm
                                          codef = IrFun fnm [clnm, xnm] ff
                                      tell [codef]
                                      return $ MkClosure fnm (map IrVar fv)

letter :: [Name] -> Ir -> Name -> Ir
letter xs t clos = foldr (\(x, i) ir -> (IrLet x (IrAccess (IrVar clos) i) ir)) t xs'
                   where xs' = zip xs [1..]

runCC :: [Decl Term] -> [IrDecl]
runCC xs = snd $ runWriter $ runStateT (go xs) 0
           where go [] = return [] 
                 go ((Decl _ x _ body) : xs) = do body' <- closureConvert body
                                                  let irDecl = IrVal x body'
                                                  tell [irDecl]
                                                  go xs