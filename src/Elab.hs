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

module Elab ( elab, elab_decl, desugar, buildTy, desugarTy ) where

import Lang
import Subst
import Global ( GlEnv(..) )
import MonadFD4

-- | 'elab' transforma variables ligadas en índices de de Bruijn
-- en un término dado. 
elab :: SNTerm -> Term
elab = elab' [] . desugar

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
elab_decl = fmap elab

-- | Transforma términos azucarados en términos sugar-free
desugar :: SNTerm -> NTerm
desugar (SV i v) = V i v
desugar (SConst i c) = Const i c
desugar (SLam i (((x : []), ty) : []) t) = Lam i x ty (desugar t)
desugar (SLam i (((x : xs), ty) : []) t) = Lam i x ty (desugar (SLam i [(xs, ty)] t))
desugar (SLam i (((x : []), ty) : xss) t) = Lam i x ty (desugar (SLam i xss t))
desugar (SLam i (((x : xs), ty) : xss) t) = Lam i x ty (desugar (SLam i ((xs, ty) : xss) t))
desugar (SApp i h a) = App i (desugar h) (desugar a)
desugar (SPrint i str t) = Print i str (desugar t)
desugar (SUPrint i str) = Lam i "x" NatTy (Print i str (V i "x"))
desugar (SBinaryOp i o t u) = BinaryOp i o (desugar t) (desugar u)
desugar (SFix i f fty x xty t) = Fix i f fty x xty (desugar t)
desugar (SIfZ i c t e) = IfZ i (desugar c) (desugar t) (desugar e)
desugar (SLet i v vty def body) = Let i v vty (desugar def) (desugar body)
desugar st@(SLetFun _ r _ _ _ _ _) = if r then desugarFunRec st else desugarLetFun st
-- desugar (SDeclTy i Name Ty) = 

-- Funciones auxiliares para desugar
desugarLetFun :: SNTerm -> NTerm
desugarLetFun (SLetFun i _ f (((x : []) , ty) : []) fty def body) = Let i f (FunTy ty fty) (Lam i x ty (desugar def)) (desugar body)
desugarLetFun (SLetFun i _ f argsf fty def body) = Let i f (buildTy argsf fty) (desugar (SLam i argsf def)) (desugar body)

desugarFunRec :: SNTerm -> NTerm
desugarFunRec (SLetFun i _ f (((x : []) , ty) : []) fty def body) = Let i f (FunTy ty fty) (Fix i f (FunTy ty fty) x ty (desugar def)) (desugar body)
desugarFunRec (SLetFun i b f (((x : xs) , ty) : []) fty def body) = desugar (SLetFun i b f [([x], ty)] (buildTy [(xs, ty)] fty) (SLam i [(xs, ty)] def) body)
desugarFunRec (SLetFun i b f (((x : []) , ty) : xss) fty def body) = desugar (SLetFun i b f [([x], ty)] (buildTy xss fty) (SLam i xss def) body)
desugarFunRec (SLetFun i b f (((x : xs) , ty) : xss) fty def body) = desugar (SLetFun i b f [([x], ty)] (buildTy ((xs, ty) : xss) fty) (SLam i ((xs, ty) : xss) def) body)

-- | Recibe los tipos de los argumentos de una función, el tipo que devuelve y construye el tipo de la función.
buildTy :: [([Name], Ty)] -> Ty -> Ty
buildTy (((x : []), ty) : []) fty = FunTy ty fty
buildTy (((x : xs), ty) : []) fty = FunTy ty (buildTy [(xs, ty)] fty)
buildTy (((x : []), ty) : xss) fty = FunTy ty  (buildTy xss fty)
buildTy (((x : xs), ty) : xss) fty = FunTy ty (buildTy ((xs, ty) : xss) fty)

-- | Dado un tipo azucarado devuelve algo del tipo Ty ??
desugarTy :: MonadFD4 m => STy -> m Ty
desugarTy SNatTy = return NatTy
desugarTy (SFunTy tx ty) = do tx' <- desugar tx
                              ty' <- desugar ty
                              return (FunTy tx' ty')
desugarTy (SDTy n) = do s <z- get
                        tn <- lookup (s)