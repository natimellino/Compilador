{-|
Module      : PPrint
Description : Pretty printer para FD4.
Copyright   : (c) Mauro Jaskelioff, Guido Martínez, 2020.
License     : GPL-3
Maintainer  : mauro@fceia.unr.edu.ar
Stability   : experimental

-}

module PPrint (
    pp,
    ppTy,
    ppName,
    ppDecl
    ) where

import Lang
import Subst ( open, openN )

import Data.Text ( unpack )
import Prettyprinter.Render.Terminal
  ( renderStrict, italicized, bold, color, colorDull, Color (..), AnsiStyle )
import Data.Text.Prettyprint.Doc
 --( (<+>), nest, parens, sep, pretty, Doc, layoutSmart, defaultLayoutOptions, annotate )
import MonadFD4
import Global

freshen :: [Name] -> Name -> Name
freshen ns n = let cands = n : (map (\i -> n ++ show i) [0..]) 
               in head (filter (\m -> not (elem m ns)) cands)

-- | 'openAll' convierte términos locally nameless
-- a términos fully named abriendo todos las variables de ligadura que va encontrando
-- Debe tener cuidado de no abrir términos con nombres que ya fueron abiertos.
-- Estos nombres se encuentran en la lista ns (primer argumento).
openAll :: [Name] -> Term -> NTerm
openAll ns (V p v) = case v of 
      Bound i ->  V p $ "(Bound "++show i++")" --este caso no debería aparecer
                                               --si el término es localmente cerrado
      Free x -> V p x
      Global x -> V p x
openAll ns (Const p c) = Const p c
openAll ns (Lam p x ty t) = 
  let x' = freshen ns x 
  in Lam p x' ty (openAll (x':ns) (open x' t))
openAll ns (App p t u) = App p (openAll ns t) (openAll ns u)
openAll ns (Fix p f fty x xty t) = 
  let 
    x' = freshen ns x
    f' = freshen (x':ns) f
  in Fix p f' fty x' xty (openAll (x:f:ns) (openN [f',x'] t))
openAll ns (IfZ p c t e) = IfZ p (openAll ns c) (openAll ns t) (openAll ns e)
openAll ns (Print p str t) = Print p str (openAll ns t)
openAll ns (BinaryOp p op t u) = BinaryOp p op (openAll ns t) (openAll ns u)
openAll ns (Let p v ty m n) = 
    let v'= freshen ns v 
    in  Let p v' ty (openAll ns m) (openAll (v':ns) (open v' n))

--Colores
constColor = annotate (color Red)
opColor = annotate (colorDull Green)
keywordColor = annotate (colorDull Green) -- <> bold)
typeColor = annotate (color Blue <> italicized)
typeOpColor = annotate (colorDull Blue)
defColor = annotate (colorDull Magenta <> italicized)
nameColor = id

-- | Pretty printer de nombres (Doc)
name2doc :: Name -> Doc AnsiStyle
name2doc n = nameColor (pretty n)

-- |  Pretty printer de nombres (String)
ppName :: Name -> String
ppName = id

-- | Pretty printer para tipos (Doc)
ty2doc :: STy -> Doc AnsiStyle
ty2doc SNatTy     = typeColor (pretty "Nat")
ty2doc (SFunTy x@(SFunTy _ _) y) = sep [parens (ty2doc x), typeOpColor (pretty "->"),ty2doc y]
ty2doc (SFunTy x y) = sep [ty2doc x, typeOpColor (pretty "->"),ty2doc y] 
ty2doc (SDTy x) = name2doc x

-- | Pretty printer para tipos (String)
ppTy :: Ty -> String
ppTy = render . ty2doc . resugarTy

c2doc :: Const -> Doc AnsiStyle
c2doc (CNat n) = constColor (pretty (show n))

binary2doc :: BinaryOp -> Doc AnsiStyle
binary2doc Add = opColor (pretty "+")
binary2doc Sub = opColor (pretty "-")

collectApp :: SNTerm -> (SNTerm, [SNTerm])
collectApp t = go [] t where
  go ts (SApp _ h tt) = go (tt:ts) h
  go ts h = (h, ts)

parenIf :: Bool -> Doc a -> Doc a
parenIf True = parens
parenIf _ = id

-- t2doc at t :: Doc
-- at: debe ser un átomo
-- | Pretty printing de términos (Doc)
t2doc :: Bool     -- Debe ser un átomo? 
      -> SNTerm    -- término a mostrar
      -> Doc AnsiStyle
-- Uncomment to use the Show instance for STerm
{- t2doc at x = text (show x) -}
t2doc at (SV _ x) = name2doc x
t2doc at (SConst _ c) = c2doc c
t2doc at (SLam _ args t) =
  parenIf at $
  sep [sep [ keywordColor (pretty "fun")
           , manyargs2doc args
           , opColor(pretty "->")]
      , nest 2 (t2doc False t)]

t2doc at t@(SApp _ _ _) =
  let (h, ts) = collectApp t in
  parenIf at $
  t2doc True h <+> sep (map (t2doc True) ts)

t2doc at (SFix _ f fty x xty m) =
  parenIf at $
  sep [ sep [keywordColor (pretty "fix")
                  , binding2doc (f, fty)
                  , binding2doc (x, xty)
                  , opColor (pretty "->") ]
      , nest 2 (t2doc False m)
      ]
t2doc at (SIfZ _ c t e) =
  parenIf at $
  sep [keywordColor (pretty "ifz"), nest 2 (t2doc False c)
     , keywordColor (pretty "then"), nest 2 (t2doc False t)
     , keywordColor (pretty "else"), nest 2 (t2doc False e) ]

t2doc at (SPrint _ str t) =
  parenIf at $
  sep [keywordColor (pretty "print"), pretty (show str), t2doc True t]

t2doc at (SUPrint _ str) =
  parenIf at $
  sep [keywordColor (pretty "print"), pretty (show str)]

t2doc at (SLet _ v ty t t') =
  parenIf at $
  sep [
    sep [keywordColor (pretty "let")
       , binding2doc (v,ty)
       , opColor (pretty "=") ]
  , nest 2 (t2doc False t)
  , keywordColor (pretty "in")
  , nest 2 (t2doc False t') ]

t2doc at (SLetFun _ r v args ty t t') =
  parenIf at $
  sep [
    sep [keywordColor (pretty "let")
       , if r then (pretty "rec") else emptyDoc
       , manyargs2doc args
       , opColor (pretty ":")
       , ty2doc ty
       , opColor (pretty "=") ]
  , nest 2 (t2doc False t)
  , keywordColor (pretty "in")
  , nest 2 (t2doc False t') ]

t2doc at (SBinaryOp _ o a b) =
  parenIf at $
  t2doc True a <+> binary2doc o <+> t2doc True b

binding2doc :: (Name, STy) -> Doc AnsiStyle
binding2doc (x, ty) =
  parens (sep [name2doc x, pretty ":", ty2doc ty])

multibinder2doc :: ([Name], STy) -> Doc AnsiStyle
multibinder2doc (xs, ty) = parens (sep [sep $ map name2doc xs, pretty ":", ty2doc ty])

manyargs2doc :: [([Name], STy)] -> Doc AnsiStyle
manyargs2doc xs = sep $ map multibinder2doc xs

-- | Pretty printing de términos (String)
pp :: MonadFD4 m => Term -> m String
-- Uncomment to use the Show instance for Term
{- pp = show -}
pp t = do
       gdecl <- gets glb
       return (render . t2doc False $ resugar $ openAll (map declName gdecl) t)

render :: Doc AnsiStyle -> String
render = unpack . renderStrict . layoutSmart defaultLayoutOptions

-- | Pretty printing de declaraciones
ppDecl :: MonadFD4 m => Decl Term Ty -> m String
ppDecl (Decl p x _ t) = do 
  gdecl <- gets glb
  return (render $ sep [defColor (pretty "let")
                       , name2doc x 
                       , defColor (pretty "=")] 
                   <+> nest 2 (t2doc False (resugar (openAll (map declName gdecl) t))))

resugarTy :: Ty -> STy
resugarTy NatTy = SNatTy
resugarTy (FunTy ty ty') = SFunTy (resugarTy ty) (resugarTy ty')
                         
resugar :: NTerm -> SNTerm
resugar (V i v) = SV i v
resugar (Const p c) = SConst p c
resugar (IfZ p c t e) = SIfZ p (resugar c) (resugar t) (resugar e)
resugar (Print i str t) = SPrint i str (resugar t)
resugar (BinaryOp i o t u) = SBinaryOp i o (resugar t) (resugar u)
resugar (App p h a) = SApp p (resugar h) (resugar a)
resugar (Lam p v ty t) = let (args, st) = rebuildLam t v (resugarTy ty)
                         in (SLam p args (resugar st))
resugar (Fix p f fty x xty t) = SFix p f (resugarTy fty) x (resugarTy xty) (resugar t)
resugar (Let p v vty def body) = (SLet p v (resugarTy vty) (resugar def) (resugar body))

rebuildLam :: NTerm -> Name -> STy -> ([([Name], STy)], NTerm)
rebuildLam nt v vty = rebuildLam' nt [([v], vty)]

rebuildLam' :: NTerm -> [([Name], STy)] -> ([([Name], STy)], NTerm)
rebuildLam' (Lam p v ty t) args@((ys, sty) : xs) = if sty' == sty
                                                   then rebuildLam' t (((ys ++ [v]), sty) : xs)
                                                   else rebuildLam' t (([v], sty') : args)
                                                   where sty' = resugarTy ty
rebuildLam' nt args = (reverse args, nt)