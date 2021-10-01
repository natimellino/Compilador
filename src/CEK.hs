module CEK ( search, val2Term ) where

import Common ( abort, Pos(..) )
import Lang
import Global ( GlEnv(..) )
import MonadFD4 ( MonadFD4, lookupDecl, failFD4, printFD4 )
import PPrint ( ppName )

-- | Semántica de operadores binarios
semOp :: BinaryOp -> Int -> Int -> Int
semOp Add x y=  x + y
semOp Sub x y = max 0 (x - y)

-- | Búsqueda en el entorno de valores
lookupEnv :: Env -> Int -> Maybe Val
lookupEnv [] _ = Nothing
lookupEnv (x:_) 0 = Just x
lookupEnv (_ : xs) i = lookupEnv xs (i - 1)

-- | Fase de búsqueda
search :: MonadFD4 m => Term -> Env -> Kont -> m Val
search (V _ (Bound i)) e k = case lookupEnv e i of
                                Nothing -> abort "Error de ejecución: Variable no encontrada"
                                Just v -> destroy v k
search (V _ (Global nm)) e k = do mtm <- lookupDecl nm 
                                  case mtm of 
                                    Nothing -> failFD4 $ "Error de ejecución: variable no declarada: " ++ ppName nm 
                                    Just t -> search t e k
search (Const _ (CNat n)) _ k = destroy (N n) k
search (Lam _ f fty t) e k = destroy (Cl (ClosFun f fty e t)) k
search (App _ t u) e k = search t e ((KArg e u) : k)
search (Print _ s t) e k = search t e ((KPrint s): k)
search (BinaryOp _ op t u) e k = search t e ((KArgBOp e op u) : k)
search (Fix _ f fty x xty t) e k = destroy (Cl (ClosFix f fty x xty e t)) k
search (IfZ _ c t u) e k = search c e ((KIfz e t u) : k)
search (Let _ x xty t u) e k = search t e ((KLet e u) : k)

-- | Fase de reducción
destroy :: MonadFD4 m => Val -> Kont -> m Val
destroy v [] = return v
destroy v ((KPrint s) : k) = do case v of
                                  N i -> do printFD4 (s++show i)
                                            destroy v k
                                  _ -> abort "Error de tipo en runtime! Print"
destroy v ((KArgBOp e op u) : k) = search u e ((KValBOp op v) : k)
destroy (N n') ((KValBOp op (N n)) : k) = destroy (N (semOp op n n')) k
destroy v' ((KValBOp _ _) : _) = abort "Error de tipo en runtime! BOp"
destroy (N 0) ((KIfz e t _) : k) = search t e k
destroy (N _) ((KIfz e _ u) : k) = search u e k
destroy v ((KIfz _ _ _) : _) = abort "Error de tipo en runtime! Ifz"
destroy (Cl clos) ((KArg e t) : k) = search t e ((KClos clos) : k)
destroy v ((KClos (ClosFun _ ty e t)) : k) = search t (v : e) k
destroy v ((KClos (cl@(ClosFix _ _ _ _ e t))) : k) = search t (v : (Cl cl) : e) k
destroy v ((KLet e t) : k) = search t (v : e) k
destroy v k = abort ("\nDestroy atascado \n" ++ "Valor: " ++ show v ++ "\n Kont: " ++ show k)

-- | Conversión de valores a términos legibles por pprinter
val2Term :: MonadFD4 m => Val -> m Term
val2Term v = case v of
              N n -> return (Const NoPos (CNat n))
              Cl clos -> return (clos2Term clos)

clos2Term :: Clos -> Term
clos2Term (ClosFun name ty _ t) = Lam NoPos name ty t
clos2Term (ClosFix f fty x xty _ t) =  Fix NoPos f fty x xty t