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

data ByteVal = I Int | Fun ByteEnv Bytecode | RA ByteEnv Bytecode deriving Show

type ByteStack = [ByteVal]

type ByteEnv = [ByteVal]

newtype Bytecode32 = BC { un32 :: [Word32] }

-- | Búsqueda en el entorno de valores
lookupEnv :: ByteEnv -> Int -> Maybe ByteVal
lookupEnv [] _ = Nothing
lookupEnv (x:_) 0 = Just x
lookupEnv (_ : xs) i = lookupEnv xs (i - 1)

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
bc (V _ (Global nm)) = failFD4 $ "Error de compilación: se encontró una variable global: " ++ ppName nm
bc (V _ (Free nm)) = failFD4 $ "Error de compilación: se encontró una variable libre: " ++ ppName nm
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
bc (IfZ _ c t e) = do cc <- bc c
                      ct <- bc t
                      ce <- bc e
                      let ct' = [FUNCTION, (length ct + 1)] ++ ct ++[RETURN]
                          ce' = [FUNCTION, (length ce + 1)] ++ ce ++[RETURN]
                      return $ ce' ++ ct' ++ cc ++ [IFZ]
bc (Let _ _ _ t t') = do ct <- bc t
                         ct' <- bc t'
                         return $ ct ++ [SHIFT] ++ ct' ++ [DROP]

type Module = [Decl Term]

bytecompileModule :: MonadFD4 m => Module -> m Bytecode
bytecompileModule p = do let tp = compact $ map (fmap (fmap global2free)) p
                         ctp <- bc tp
                         return $ ctp ++ [PRINTN, STOP]

compact :: Module -> Term
compact ((Decl i nm ty b):[]) = Let i nm ty b (V i (Bound 0))
compact ((Decl i nm ty b) : ds) = let b' = compact ds
                                  in Let i nm ty b $ close nm b'

global2free :: Var -> Var
global2free (Global nm) = Free nm
global2free v = v

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
runBC c = runBC' c [] []

runBC' :: MonadFD4 m => Bytecode -> ByteEnv -> ByteStack -> m ()
runBC' (CONST : n : c) e s = runBC' c e ((I n) : s)
runBC' (ADD : c) e ((I n) : (I m) : s) = runBC' c e ((I $ m + n) : s)
runBC' (SUB : c) e ((I n) : (I m) : s) = runBC' c e ((I $ m - n) : s)
runBC' (ACCESS : i : c) e s = case lookupEnv e i of
                                Just v -> runBC' c e (v : s)
                                Nothing -> failFD4 $ "Error de ejecución, variable no encontrada en el entorno"
runBC' (CALL : c) e (v : (Fun ef cf) : s) = runBC' cf (v : ef) ((RA e c) : s)
runBC' (FUNCTION : l : c) e s = runBC' (drop l c) e ((Fun e $ take l c) : s)
runBC' (RETURN : _) _ (v : (RA e c) : s) = runBC' c e (v : s)
runBC' (SHIFT : c) e (v : s) = runBC' c (v : e) s
runBC' (DROP : c) (v : e) s = runBC' c e s
runBC' (PRINTN : c) e s@((I n) : _) = do printFD4 $ show n
                                         runBC' c e s
runBC' (PRINT : c) e s = do let (str, k) = break (== NULL) c
                                str' = map chr str
                            printFD4 str'
                            runBC' k e s
runBC' (FIX : c) e ((Fun ef cf) : s) = do let efix = (Fun (efix : ef) cf)
                                          runBC' c e (efix : s)
runBC' (IFZ : c) e ((I 0) : (Fun et ct) : _ : s) = runBC' ct et ((RA et c) : s)
runBC' (IFZ : c) e ((I _) : _ : (Fun ee ce) : s) = runBC' ce ee ((RA ee c) : s)
runBC' (STOP : _) _ _ = return ()
runBC' _ _ _ = failFD4 "Cómo llegaste acá?"
