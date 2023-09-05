module Optimize ( optimize ) where

import Lang
import MonadFD4
import Eval ( semOp )
import Subst ( subst, varChanger )

optimize :: MonadFD4 m => Term -> m Term
optimize t = do opt <- checkOptim
                n <- getOptimIters
                if opt then optimizeN n t
                       else return t

-- foldr (>=>) return
optimizeN :: MonadFD4 m => Int -> Term -> m Term
optimizeN 0 t = return t
optimizeN n t = do t1 <- constantFolding t
                   t2 <- constantPropagation t1
                   if t2 /= t then optimizeN (n - 1) t2
                              else return t

constantFolding :: MonadFD4 m => Term -> m Term
constantFolding (BinaryOp i op t t') = 
  do tt  <- constantFolding t
     tt' <- constantFolding t'
     case (tt, tt', op) of 
       (_, (Const _ (CNat 0)), _) -> return tt
       ((Const _ (CNat 0)), _, Add) -> return tt'
       ((Const _ (CNat n)), (Const _ (CNat m)), _) -> return $ Const i $ CNat (semOp op n m)
       _  -> return $ BinaryOp i op tt tt'
constantFolding (IfZ i c t e) = 
  do tc <- constantFolding c
     tt <- constantFolding t
     te <- constantFolding e
     case tc of
       (Const _ (CNat 0)) -> return tt
       (Const _ (CNat _)) -> return te
       _                  -> if tt == te then return tt
                                         else return $ (IfZ i tc tt te)
constantFolding t = visitor constantFolding t

constantPropagation :: MonadFD4 m => Term -> m Term 
constantPropagation (Let i x xty t u) = 
  do tt <- constantPropagation t
     tu <- constantPropagation u
     case tt of 
      c@(Const _ _) -> return $ (subst' c tu)
      _ -> return $ Let i x xty tt tu
constantPropagation t = visitor constantPropagation t

--  Función alternativa a subst porque en constantPropagation estamos haciendo
-- substitución con terminos fuera del scope actual. (Bound mayores a la profundidad)
subst' :: Term -> Term -> Term
subst' t = varChanger (\_ p n -> V p (Free n)) bnd
   where ns = [t]
         bnd depth p i 
            | i <  depth = V p (Bound i)
            | i >= depth && i < depth + nns
               = nsr !! (i - depth)
            | otherwise = V p (Bound $ i - 1)
         nns = length ns
         nsr = reverse ns


hasPrint :: Term -> Bool
hasPrint (Print _ _ t     ) = True
hasPrint (V _ _           ) = False
hasPrint (Const _ _       ) = False
hasPrint (Lam _ _ _ t     ) = hasPrint t
hasPrint (App   _ l r     ) = hasPrint l || hasPrint r
hasPrint (BinaryOp _ _ t u) = hasPrint t || hasPrint u
hasPrint (Fix _ _ _ _ _ t ) = hasPrint t
hasPrint (IfZ _ c t e     ) = hasPrint c || hasPrint t || hasPrint e
hasPrint (Let _ _ _ e t   ) = hasPrint t

visitor :: MonadFD4 m => (Term -> m Term) -> Term -> m Term
visitor _ t@(Const _ _) = return t                                          
visitor _ t@(V _ _) = return t
visitor f (BinaryOp i op t t') = do tt  <- f t
                                    tt' <- f t'
                                    return $ BinaryOp i op tt tt'
visitor f (Lam i n ty t) = do tt <- f t
                              return $ Lam i n ty tt
visitor f (App i t t') = do tt <- f t
                            tt' <- f t'
                            return $ App i tt tt'
visitor f (Print i s t) = do tt <- f t
                             return $ Print i s tt
visitor f (Fix i n ty n' ty' t) = do tt <- f t
                                     return $ Fix i n ty n' ty' tt
visitor f (IfZ i c t e) = do tc <- f c
                             tt <- f t
                             te <- f e
                             return $ IfZ i tc tt te            
visitor f (Let i n ty t t') = do tt <- f t
                                 tt' <- f t'
                                 return $ Let i n ty tt tt'
