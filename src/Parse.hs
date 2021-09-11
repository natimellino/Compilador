{-|
Module      : Parse
Description : Define un parser de términos FD40 a términos fully named.
Copyright   : (c) Mauro Jaskelioff, Guido Martínez, 2020.
License     : GPL-3
Maintainer  : mauro@fceia.unr.edu.ar
Stability   : experimental

-}

module Parse (tm, Parse.parse, decl, runP, P, program, declOrTm, parseDecl) where

import Prelude hiding ( const )
import Lang
import Common
import Text.Parsec hiding (runP,parse)
import Data.Char ( isNumber, ord )
import qualified Text.Parsec.Token as Tok
import Text.ParserCombinators.Parsec.Language --( GenLanguageDef(..), emptyDef )
import qualified Text.Parsec.Expr as Ex
import Text.Parsec.Expr (Operator, Assoc)
import Control.Monad.Identity (Identity)

type P = Parsec String ()

-----------------------
-- Lexer
-----------------------
-- | Analizador de Tokens
lexer :: Tok.TokenParser u
lexer = Tok.makeTokenParser $
        emptyDef {
         commentLine    = "#",
         reservedNames = ["let", "fun", "fix", "then", "else","in", 
                           "ifz", "print","Nat","rec","type"],
         reservedOpNames = ["->",":","=","+","-"]
        }

whiteSpace :: P ()
whiteSpace = Tok.whiteSpace lexer

natural :: P Integer 
natural = Tok.natural lexer

stringLiteral :: P String
stringLiteral = Tok.stringLiteral lexer

parens :: P a -> P a
parens = Tok.parens lexer

identifier :: P String
identifier = Tok.identifier lexer

reserved :: String -> P ()
reserved = Tok.reserved lexer

reservedOp :: String -> P ()
reservedOp = Tok.reservedOp lexer

-----------------------
-- Parsers
-----------------------

num :: P Int
num = fromInteger <$> natural

var :: P Name
var = identifier 

getPos :: P Pos
getPos = do pos <- getPosition
            return $ Pos (sourceLine pos) (sourceColumn pos)

tyatom :: P Ty
tyatom = (reserved "Nat" >> return NatTy)
         <|> parens typeP

typeP :: P Ty
typeP = try (do 
          x <- tyatom
          reservedOp "->"
          y <- typeP
          return (FunTy x y))
      <|> tyatom
          
const :: P Const
const = CNat <$> num

printOp :: P SNTerm
printOp = do
  i <- getPos
  reserved "print"
  str <- option "" stringLiteral
  (do a <- atom
      return (SPrint i str a)
      <|>
      return (SUPrint i str))

binary :: String -> BinaryOp -> Assoc -> Operator String () Identity SNTerm
binary s f = Ex.Infix (reservedOp s >> return (SBinaryOp NoPos f))

table :: [[Operator String () Identity SNTerm]]
table = [[binary "+" Add Ex.AssocLeft,
          binary "-" Sub Ex.AssocLeft]]

expr :: P SNTerm
expr = Ex.buildExpressionParser table tm

atom :: P SNTerm
atom =     (flip SConst <$> const <*> getPos)
       <|> flip SV <$> var <*> getPos
       <|> parens expr
       <|> printOp

-- parsea un par (variable : tipo)
binding :: P (Name, Ty)
binding = do v <- var
             reservedOp ":"
             ty <- typeP
             return (v, ty)

multibinding :: P ([Name], Ty)
multibinding = do v <- many var
                  reservedOp ":"
                  ty <- typeP
                  return (v, ty)

lam :: P SNTerm
lam = do i <- getPos
         reserved "fun"
         args <- many (parens multibinding)
         reservedOp "->"
         t <- expr
         return (SLam i args t)

-- Nota el parser app también parsea un solo atom.
app :: P SNTerm
app = (do i <- getPos
          f <- atom
          args <- many atom
          return (foldl (SApp i) f args))

ifz :: P SNTerm
ifz = do i <- getPos
         reserved "ifz"
         c <- expr
         reserved "then"
         t <- expr
         reserved "else"
         e <- expr
         return (SIfZ i c t e)

fix :: P SNTerm
fix = do i <- getPos
         reserved "fix"
         (f, fty) <- parens binding
         (x, xty) <- parens binding
         reservedOp "->"
         t <- expr
         return (SFix i f fty x xty t)

letexp :: P SNTerm
letexp = do i <- getPos
            reserved "let"
            (v,ty) <- parens binding <|> binding
            reservedOp "="  
            def <- expr
            reserved "in"
            body <- expr
            return (SLet i v ty def body)

-- TODO: ver tema de los args
letexpfun :: P SNTerm
letexpfun = do i <- getPos
               reserved "let"
               (do reserved "rec"
                   letexpfun' i True
                   <|>
                   letexpfun' i False)
            where letexpfun' i isRec = do f <- var
                                          args <- many (parens multibinding)
                                          reservedOp ":"
                                          t <- typeP
                                          reservedOp "="
                                          def <- expr
                                          reserved "in"
                                          body <- expr
                                          if not (null args) -- Verif. de que tenga args
                                          then return (SLetFun i isRec f args t def body)
                                          else error ("no args for function: " ++ show f)                             

-- | Parser de términos
tm :: P SNTerm
tm = app <|> lam <|> ifz <|> printOp <|> fix <|> (try letexp <|> letexpfun)

-- | Parser de declaraciones
decl :: P (Decl SNTerm)
decl = try declvar <|> (try declfunrec <|> declfun)

declvar :: P (Decl SNTerm)
declvar = do i <- getPos
             reserved "let"
             (v,_) <- parens binding <|> binding
             reservedOp "="  
             t <- expr
             return (Decl i v t)

declfun :: P (Decl SNTerm)
declfun = do i <- getPos
             reserved "let"
             v <- var
             args <- many (parens multibinding)
             reservedOp ":"
             typeP
             reservedOp "="
             t <- expr
             return (Decl i v (SLam i args t))

declfunrec :: P (Decl SNTerm)
declfunrec = do i <- getPos
                reserved "let"
                reserved "rec"
                v <- var
                args <- many (parens multibinding)
                reservedOp ":"
                ty <- typeP
                reservedOp "="
                t <- expr
                case args of
                    [] -> error ("no args for function: " ++ show v)
                    (((x : []), ty') : []) -> return (Decl i v ((SFix i v ty x ty' t)))
                    (((x : xs), ty') : []) -> return (Decl i v ((SFix i v ty x ty' (SLam i [(xs, ty')] t))))
                    (((x : []), ty') : xss) -> return (Decl i v (SFix i v ty x ty' (SLam i xss t)))
                    (((x : xs), ty') : xss) -> return (Decl i v ((SFix i v ty x ty' (SLam i ((xs, ty') : xss) t))))                                            

-- | Parser de programas (listas de declaraciones) 
program :: P [Decl SNTerm]
program = many decl

-- | Parsea una declaración a un término
-- Útil para las sesiones interactivas
declOrTm :: P (Either (Decl SNTerm) SNTerm)
declOrTm =  try (Left <$> decl) <|> (Right <$> expr)

-- Corre un parser, chequeando que se pueda consumir toda la entrada
runP :: P a -> String -> String -> Either ParseError a
runP p s filename = runParser (whiteSpace *> p <* eof) () filename s

--para debugging en uso interactivo (ghci)
parse :: String -> SNTerm
parse s = case runP expr s "" of
            Right t -> t
            Left e -> error ("no parse: " ++ show s)

parseDecl :: String -> (Decl SNTerm)
parseDecl s = case runP decl s "" of
                Right t -> t
                Left e -> error ("no parse: " ++ show s)

{-
TEST CASES FOR PARSER:

NO DECLARATIONS
parse "let f (x:Nat):Nat = x+2 in f 1"
parse "let (x:Nat) = 2 in x+x"
parse "let x:Nat = 2 in x+x"
parse "let rec f (x:Nat):Nat = x+2 in f 1"

DECLARATIONS:
parseDecl "let (x:Nat) = 2"
parseDecl "let x:Nat = 2"
parseDecl "let rec f (x:Nat):Nat = x+2"
parseDecl "let f (x:Nat):Nat = x+2"
parseDecl "let rec f (x y:Nat) (z : Nat ):Nat = x+2"
parseDecl "let rec f (x y:Nat):Nat = x+2"
-}