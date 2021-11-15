module C ( ir2C ) where
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Terminal ( renderStrict )
import IR
import Lang
import Data.Text (unpack)

decl2doc :: IrDecl -> Doc a
decl2doc (IrVal n t) = pretty "void*" <+> name n <> semi
decl2doc (IrFun n args t) =
  pretty "void*" <+> name n <+> tupled (map (\x -> pretty "void**" <+> name x) args) <+>
  braces (nest 2 (line <> pretty "return" <+> voidptr <> parens (ir2doc t) <> semi) <> line)

fd4Main :: [IrDecl] -> Doc a
fd4Main xs = pretty "uint64_t* fd4main()" 
         <+> braces (nest 2 (line <> vsep (vals2doc xs ++ [pretty "return 0;"])) <> line)
  where vals2doc :: [IrDecl] -> [Doc a]
        vals2doc []               = []
        vals2doc [IrVal n t]      = [name n <+> pretty "=" <+> voidptr <> parens (ir2doc t) <> semi, irPrintN (name n), semi]
        vals2doc (IrVal n t : ds) = (name n <+> pretty "=" <+> voidptr <> parens (ir2doc t) <> semi) : vals2doc ds
        vals2doc (_ : ds)         = vals2doc ds

name :: String -> Doc a
name n = pretty $ "fd4_"++n    --prefijo fd4 para evitar colision con nombres de C.

stmt :: Doc a -> Doc a
stmt x = parens (braces (nest 2 (line <> x <> semi) <> line))

stmts:: [Doc a] -> Doc a
stmts xs = parens $ braces $ 
     foldr (\x ds -> nest 2 (line <> x <> semi) <> ds) line xs

u64 :: Doc a
u64 = parens (pretty "uint64_t")

voidptr :: Doc a
voidptr = parens (pretty "void *")

ir2doc :: Ir -> Doc a
ir2doc (IrVar n) = name n
ir2doc (IrGlobal n) = name n
ir2doc (IrCall f args) = parens (pretty "(void* (*) (void*, void*))" <+> ir2doc f) <> tupled (map (( voidptr<>) . ir2doc) args)
ir2doc (IrConst (CNat n)) = pretty n
ir2doc (IrBinaryOp Add a b) = u64 <+> ir2doc a <+> pretty "+" <+> u64 <+> ir2doc b
ir2doc (IrBinaryOp Sub a b) = stmts [pretty "fd4_sub" <> tupled [u64 <+> ir2doc a, u64 <+> ir2doc b]]
ir2doc (IrLet n t t') = stmts [hsep [pretty "void**", name n, pretty "=",  ir2doc t] <> semi <> line <> ir2doc t']
ir2doc (IrIfZ c a b) = sep [ir2doc c, nest 2 (pretty "?" <+> voidptr <> ir2doc b), nest 2 (colon <+> voidptr <> ir2doc a)]
ir2doc (IrPrint str t) = stmts [pretty "wprintf" <> parens (pretty "L" <> pretty (show str)),irPrintN (ir2doc t)]
ir2doc (MkClosure f args) = pretty "fd4_mkclosure" <> tupled (name f : pretty (length args) : map ir2doc args)
ir2doc (IrAccess t i) = parens (ir2doc t) <> brackets (pretty i)

op2doc :: BinaryOp -> Doc a
op2doc Add = pretty "+"
op2doc Sub = pretty "-"

prelude :: Doc a
prelude = pretty "#include <inttypes.h>" 
       <> line
       <> pretty "#include <wchar.h>" 
       <> line
       <> pretty "extern void *fd4_mkclosure(void*, int, ...);"
       <> line
       <> pretty "extern void *fd4_printn(uint64_t);"
       <> line
       <> pretty "extern void *fd4_sub(uint64_t, uint64_t);"
       <> line

irPrintN :: Doc a -> Doc a
irPrintN x = pretty "fd4_printn" <> parens (u64 <> x)

ir2C :: IrDecls -> String
ir2C (IrDecls xs) = unpack . renderStrict . layoutSmart defaultLayoutOptions $ vsep (prelude : map decl2doc xs ++ [fd4Main xs])