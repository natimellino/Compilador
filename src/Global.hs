{-|
Module      : Global
Description : Define el estado global del compilador
Copyright   : (c) Mauro Jaskelioff, Guido Martínez, 2020.
License     : GPL-3
Maintainer  : mauro@fceia.unr.edu.ar
Stability   : experimental

-}
module Global where

import Lang

-- TODO: preguntar si podemos usar tyEnv para las declaraciones de tipo o no

data GlEnv = GlEnv {
  inter :: Bool,        -- ^ True, si estamos en modo interactivo.
  lfile :: String,      -- ^ Último archivo cargado.
  cantDecl :: Int,      -- ^ Cantidad de declaraciones desde la última carga
  glb :: [Decl Term],   -- ^ Entorno con declaraciones globales
  tyEnv :: [(Name,Ty)],  -- ^ Entorno de tipado de declaraciones globales
  styEnv :: [(Name,Ty)] -- ^ Entorno de tipado de declaraciones de tipo azucaradas
}

-- | Valor del estado inicial
initialEnv :: GlEnv
initialEnv = GlEnv True "" 0 [] [] []
