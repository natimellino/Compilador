{-# LANGUAGE PatternSynonyms #-}
{-|
Module      : Byecompile
Description : Compila a bytecode. Ejecuta bytecode.
Copyright   : (c) Mauro Jaskelioff, Guido Martínez, 2020.
License     : GPL-3
Maintainer  : mauro@fceia.unr.edu.ar
Stability   : experimental

Este módulo permite compilar módulos a la BVM. También provee una implementación de la BVM 
para ejecutar bytecode.
-}
module Bytecompile
  (Bytecode, runBC, bcWrite, bcRead, bytecompileModule)
 where

import Lang 
import Subst
import MonadFD4
import PPrint ( ppName )

import qualified Data.ByteString.Lazy as BS
import Data.Binary ( Word32, Binary(put, get), decode, encode )
import Data.Binary.Put ( putWord32le )
import Data.Binary.Get ( getWord32le, isEmpty )

import Data.Char

type Opcode = Int
type Bytecode = [Opcode]

newtype Bytecode32 = BC { un32 :: [Word32] }

{- Esta instancia explica como codificar y decodificar Bytecode de 32 bits -}
instance Binary Bytecode32 where
  put (BC bs) = mapM_ putWord32le bs
  get = go 
    where go =  
           do
            empty <- isEmpty
            if empty
              then return $ BC []
              else do x <- getWord32le
                      BC xs <- go
                      return $ BC (x:xs)

{- Estos sinónimos de patrón nos permiten escribir y hacer
pattern-matching sobre el nombre de la operación en lugar del código
entero, por ejemplo:
 
   f (CALL : cs) = ...

 Notar que si hubieramos escrito algo como
   call = 5
 no podríamos hacer pattern-matching con `call`.

 En lo posible, usar estos códigos exactos para poder ejectutar un
 mismo bytecode compilado en distintas implementaciones de la máquina.
-}
pattern NULL     = 0
pattern RETURN   = 1
pattern CONST    = 2
pattern ACCESS   = 3
pattern FUNCTION = 4
pattern CALL     = 5
pattern ADD      = 6
pattern SUB      = 7
pattern IFZ      = 8
pattern FIX      = 9
pattern STOP     = 10
pattern SHIFT    = 11
pattern DROP     = 12
pattern PRINT    = 13
pattern PRINTN   = 14

bc :: MonadFD4 m => Term -> m Bytecode
bc (V _ (Bound i)) = return [ACCESS, i]
bc (V _ _) = failFD4 $ "Error de compilación: se encontró una variable libre o global"
bc (Const _ (CNat n)) = return [CONST, n]
bc (Lam _ _ _ t) = do ct <- bc t
                      let ft = ct ++ [RETURN]
                      return $ [FUNCTION, length ft] ++ ft                   
bc (App _ f e) = do cf <- bc f
                    ce <- bc e
                    return $ cf ++ ce ++ [CALL]
bc (Print _ str t) = do let ordStr = map ord str
                        ct <- bc t
                        return $ [PRINT] ++ ordStr ++ [NULL] ++ ct ++ [PRINTN]
bc (BinaryOp _ Add t t') = do ct <- bc t
                              ct' <- bc t'
                              return $ ct ++ ct' ++ [ADD]
bc (BinaryOp _ Sub t t') = do ct <- bc t
                              ct' <- bc t'
                              return $ ct ++ ct' ++ [SUB]
bc (Fix _ _ _ _ _ t) = do ct <- bc t
                          let ft = ct ++ [RETURN]
                          return $ [FUNCTION, length ft] ++ ft ++ [FIX]
bc (IfZ _ c t f) = undefined
bc (Let _ _ _ t t') = do ct <- bc t
                         ct' <- bc t'
                         return $ ct ++ [SHIFT] ++ ct' ++ [DROP]

type Module = [Decl Term]

bytecompileModule :: MonadFD4 m => Module -> m Bytecode
bytecompileModule p = do let (tp, nms) = compact p []
                             tp' = global2free tp
                         ctp <- bc (closeN nms tp')
                         return $ ctp ++ [PRINTN, STOP]

compact :: Module -> [Name] -> (Term, [Name])
compact ((Decl i nm ty b):[]) nms = (Let i nm ty b (V i (Free nm)), [nm])
compact ((Decl i nm ty b) : ds) nms = let (b', nms') = compact ds nms
                                      in (Let i nm ty b b', (nm : nms'))

global2free :: Term -> Term
global2free (V i (Global nm)) = (V i (Free nm))
global2free v@(V _ _) = v 
global2free c@(Const _ _) = c
global2free (Lam p nm ty t) = Lam p nm ty $ global2free t           
global2free (App p f e) = App p (global2free f) (global2free e)
global2free (Print p str t) = Print p str $ global2free t
global2free (BinaryOp p op t t') = BinaryOp p op (global2free t) (global2free t')
global2free (Fix p f fty x xty t) = Fix p f fty x xty (global2free t)
global2free (IfZ p c t f) = IfZ p (global2free c) (global2free t) (global2free f)
global2free (Let i nm ty t t') = Let i nm ty (global2free t) (global2free t')  

-- | Toma un bytecode, lo codifica y lo escribe un archivo 
bcWrite :: Bytecode -> FilePath -> IO ()
bcWrite bs filename = BS.writeFile filename (encode $ BC $ fromIntegral <$> bs)

---------------------------
-- * Ejecución de bytecode
---------------------------

-- | Lee de un archivo y lo decodifica a bytecode
bcRead :: FilePath -> IO Bytecode
bcRead filename = map fromIntegral <$> un32  <$> decode <$> BS.readFile filename

runBC :: MonadFD4 m => Bytecode -> m ()
runBC c = error "implementame"
