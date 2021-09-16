{-|
Module      : Elab
Description : Elabora un término fully named a uno locally closed.
Copyright   : (c) Mauro Jaskelioff, Guido Martínez, 2020.
License     : GPL-3
Maintainer  : mauro@fceia.unr.edu.ar
Stability   : experimental

Este módulo permite elaborar términos y declaraciones para convertirlas desde
fully named (@NTerm) a locally closed (@Term@) y convertir desde azucarados
fully name (@SNTerm) a solamente fully named (@NTerm).
-}

module Elab ( elab, elab_decl, desugar, buildTy, desugarTy, buildSTy) where

import Lang
import Subst
import Global ( GlEnv(..) )
import MonadFD4

-- | 'elab' transforma variables ligadas en índices de de Bruijn
-- en un término dado. 

elab :: MonadFD4 m => SNTerm -> m Term
elab n = do t <- desugar n
            return $ elab' [] t

elab' :: [Name] -> NTerm -> Term
elab' env (V p v) =
  -- Tenemos que hver si la variable es Global o es un nombre local
  -- En env llevamos la lista de nombres locales.
  if v `elem` env 
    then  V p (Free v)
    else V p (Global v)

elab' _ (Const p c) = Const p c
elab' env (Lam p v ty t) = Lam p v ty (close v (elab' (v:env) t))
elab' env (Fix p f fty x xty t) = Fix p f fty x xty (closeN [f, x] (elab' (x:f:env) t))
elab' env (IfZ p c t e)         = IfZ p (elab' env c) (elab' env t) (elab' env e)
-- Operador Print
elab' env (Print i str t) = Print i str (elab' env t)
-- Operadores binarios
elab' env (BinaryOp i o t u) = BinaryOp i o (elab' env t) (elab' env u)
-- Aplicaciones generales
elab' env (App p h a) = App p (elab' env h) (elab' env a)
elab' env (Let p v vty def body) = Let p v vty (elab' env def) (close v (elab' (v:env) body))

elab_decl :: Decl SNTerm -> Decl Term
elab_decl = undefined

-- | Transforma términos azucarados en términos sugar-free
desugar :: MonadFD4 m => SNTerm -> m NTerm
desugar (SV i v) = return (V i v)
desugar (SConst i c) = return (Const i c)
desugar (SLam i (((x : []), ty) : []) t) = do t' <- desugar t
                                              ty' <- desugarTy ty
                                              return (Lam i x ty' t')
desugar (SLam i (((x : xs), ty) : []) t) = do t' <- desugar (SLam i [(xs, ty)] t)
                                              ty' <- desugarTy ty
                                              return (Lam i x ty' t')
desugar (SLam i (((x : []), ty) : xss) t) = do t' <- desugar (SLam i xss t) 
                                               ty' <- desugarTy ty
                                               return (Lam i x ty' t')
desugar (SLam i (((x : xs), ty) : xss) t) = do t' <- desugar (SLam i ((xs, ty) : xss) t)
                                               ty' <- desugarTy ty
                                               return (Lam i x ty' t')
desugar (SApp i h a) = do h' <- desugar h 
                          a' <- desugar a
                          return (App i h' a')
desugar (SPrint i str t) = do t' <- desugar t
                              return (Print i str t')
desugar (SUPrint i str) = return (Lam i "x" NatTy (Print i str (V i "x")))
desugar (SBinaryOp i o t u) = do t' <- desugar t 
                                 u' <- desugar u
                                 return (BinaryOp i o t' u')
desugar (SFix i f fty x xty t) = do t' <- desugar t
                                    fty' <- desugarTy fty
                                    xty' <- desugarTy xty
                                    return (Fix i f fty' x xty' t')
desugar (SIfZ i c t e) = do c' <- desugar c
                            t' <- desugar t
                            e' <- desugar e
                            return (IfZ i c' t' e')
desugar (SLet i v vty def body) = do def' <- desugar def
                                     body' <- desugar body
                                     vty' <- desugarTy vty
                                     return (Let i v vty' def' body')
desugar st@(SLetFun _ r _ _ _ _ _) = do if r then desugarFunRec st else desugarLetFun st

-- Funciones auxiliares para desugar
desugarLetFun :: MonadFD4 m => SNTerm -> m NTerm
desugarLetFun (SLetFun i _ f (((x : []) , ty) : []) fty def body) = do def' <- desugar def
                                                                       body' <- desugar body
                                                                       ty' <- desugarTy ty
                                                                       fty' <- desugarTy fty
                                                                       return (Let i f (FunTy ty' fty') (Lam i x ty' def') body')
desugarLetFun (SLetFun i _ f argsf fty def body) = do body' <- desugar body
                                                      def' <- desugar (SLam i argsf def)
                                                      argsf' <- desugarManySTy argsf
                                                      fty' <- desugarTy fty
                                                      return (Let i f (buildTy argsf' fty') def' body')

desugarFunRec :: MonadFD4 m => SNTerm -> m NTerm
desugarFunRec (SLetFun i _ f (((x : []) , ty) : []) fty def body) = do def' <- desugar def
                                                                       body' <- desugar body
                                                                       fty' <- desugarTy fty
                                                                       ty' <- desugarTy ty
                                                                       return (Let i f (FunTy ty' fty') (Fix i f (FunTy ty' fty') x ty' def') body')
desugarFunRec (SLetFun i b f (((x : xs) , ty) : []) fty def body) = do desugar (SLetFun i b f [([x], ty)] (buildSTy [(xs, ty)] fty) (SLam i [(xs, ty)] def) body)
desugarFunRec (SLetFun i b f (((x : []) , ty) : xss) fty def body) = do desugar (SLetFun i b f [([x], ty)] (buildSTy xss fty) (SLam i xss def) body)
desugarFunRec (SLetFun i b f (((x : xs) , ty) : xss) fty def body) = do desugar (SLetFun i b f [([x], ty)] (buildSTy ((xs, ty) : xss) fty) (SLam i ((xs, ty) : xss) def) body)

-- | Recibe los tipos de los argumentos de una función, el tipo que devuelve y construye el tipo de la función.
buildTy :: [([Name], Ty)] -> Ty -> Ty
buildTy (((x : []), ty) : []) fty = FunTy ty fty
buildTy (((x : xs), ty) : []) fty = FunTy ty (buildTy [(xs, ty)] fty)
buildTy (((x : []), ty) : xss) fty = FunTy ty  (buildTy xss fty)
buildTy (((x : xs), ty) : xss) fty = FunTy ty (buildTy ((xs, ty) : xss) fty)

buildSTy :: [([Name], STy)] -> STy -> STy
buildSTy (((x : []), ty) : []) fty = SFunTy ty fty
buildSTy (((x : xs), ty) : []) fty = SFunTy ty (buildSTy [(xs, ty)] fty)
buildSTy (((x : []), ty) : xss) fty = SFunTy ty (buildSTy xss fty)
buildSTy (((x : xs), ty) : xss) fty = SFunTy ty (buildSTy ((xs, ty) : xss) fty)


-- | Dado un tipo azucarado devuelve algo del tipo Ty ??
desugarTy :: MonadFD4 m => STy -> m Ty
desugarTy SNatTy = return NatTy
desugarTy (SFunTy tx ty) = do tx' <- desugarTy tx
                              ty' <- desugarTy ty
                              return (FunTy tx' ty')
desugarTy (SDTy n) = do t <- lookupSTy n
                        case t of 
                          Nothing -> failFD4 $ "Error de ejecución: tipo no declarado: " ++ n
                          Just ty -> return ty

desugarManySTy :: MonadFD4 m => [([Name], STy)] -> m [([Name], Ty)]
desugarManySTy [] = return []
desugarManySTy ((x, ty) : xs) = do ty' <- desugarTy ty
                                   xs' <- desugarManySTy xs
                                   return ((x, ty') : xs')