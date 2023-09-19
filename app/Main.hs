{-# LANGUAGE RecordWildCards #-}

{-|
Module      : Main
Description : Compilador de FD4.
Copyright   : (c) Mauro Jaskelioff, Guido Martínez, 2020.
License     : GPL-3
Maintainer  : mauro@fceia.unr.edu.ar
Stability   : experimental

-}

module Main where

import System.Console.Haskeline ( defaultSettings, getInputLine, runInputT, InputT )
import Control.Monad.Catch (MonadMask)

--import Control.Monad
import Control.Monad.Trans
import Data.List (nub,  intersperse, isPrefixOf )
import Data.Char ( isSpace )
import Control.Exception ( catch , IOException )
import System.IO ( hPrint, stderr, hPutStrLn )
import System.FilePath ( splitExtension )

import System.Exit
--import System.Process ( system )
import Options.Applicative
--import Data.Text.Lazy (unpack)

import Global ( GlEnv(..) )
import Errors
import Lang
import Parse ( P, tm, program, declOrTm, runP )
import Elab ( elab, desugar, desugarDecl, desugarTy )
import Eval ( eval )
import CEK ( search, val2Term )
import PPrint ( pp , ppTy, ppDecl )
import MonadFD4
import TypeChecker ( tc, tcDecl )
import Bytecompile ( bytecompileModule, runBC, bcRead, bcWrite )
import Optimize ( optimize )
import ClosureConvert ( runCC )
import C ( ir2C )

prompt :: String
prompt = "FD4> "

{- 
 Tipo para representar las banderas disponibles en línea de comando.
-}
data Mode =
    Interactive
  | Typecheck
  | InteractiveCEK
  | Bytecompile 
  | RunVM
  | CC
  -- | Canon
  -- | LLVM
  -- | Build

-- | Parser de banderas
parseMode :: Parser (Mode,Bool)
parseMode = (,) <$> 
      (flag' Typecheck ( long "typecheck" <> short 't' <> help "Chequear tipos e imprimir el término")
      <|> flag' InteractiveCEK (long "interactiveCEK" <> short 'k' <> help "Ejecutar interactivamente en la CEK")
      <|> flag' Bytecompile (long "bytecompile" <> short 'm' <> help "Compilar a la BVM")
      <|> flag' RunVM (long "runVM" <> short 'r' <> help "Ejecutar bytecode en la BVM")
      <|> flag Interactive Interactive ( long "interactive" <> short 'i' <> help "Ejecutar en forma interactiva")
      <|> flag' CC ( long "cc" <> short 'c' <> help "Compilar a código C")
      )
   -- <*> pure False
   -- reemplazar por la siguiente línea para habilitar opción
   <*> flag False True (long "optimize" <> short 'o' <> help "Optimizar código")
  
-- | Parser de opciones general, consiste de un modo y una lista de archivos a procesar
parseArgs :: Parser (Mode,Bool, [FilePath])
parseArgs = (\(a,b) c -> (a,b,c)) <$> parseMode <*> many (argument str (metavar "FILES..."))

main :: IO ()
main = execParser opts >>= go
  where
    opts = info (parseArgs <**> helper)
      ( fullDesc
     <> progDesc "Compilador de FD4"
     <> header "Compilador de FD4 de la materia Compiladores 2021" )

    go :: (Mode,Bool,[FilePath]) -> IO ()
    go (Interactive,opt,files) = 
              do runFD4 opt (runInputT defaultSettings (repl files Interactive))
                 return ()
    go (Typecheck,opt, files) =
              runOrFail opt $ mapM_ typecheckFile files              
    go (InteractiveCEK,opt, files) =
              do runFD4 opt (runInputT defaultSettings (repl files InteractiveCEK))
                 return ()
    go (Bytecompile,opt, files) =
              runOrFail opt $ mapM_ bytecompileFile files
    go (RunVM,opt,files) =
              runOrFail opt $ mapM_ bytecodeRun files
    go (CC,opt, files) =
              runOrFail opt $ mapM_ ccFile files   

runOrFail :: Bool -> FD4 a -> IO a
runOrFail b m = do
  r <- runFD4 b m
  case r of
    Left err -> do
      liftIO $ hPrint stderr err
      exitWith (ExitFailure 1)
    Right v -> return v

repl :: (MonadFD4 m, MonadMask m) => [FilePath] -> Mode -> InputT m ()
repl args mode = do
       lift $ catchErrors $ compileFiles args mode
       s <- lift get
       when (inter s) $ liftIO $ putStrLn
         (  "Entorno interactivo para FD4.\n"
         ++ "Escriba :? para recibir ayuda.")
       loop
  where loop = do
           minput <- getInputLine prompt
           case minput of
               Nothing -> return ()
               Just "" -> loop
               Just x -> do
                       c <- liftIO $ interpretCommand x
                       b <- lift $ catchErrors $ handleCommand c mode
                       maybe loop (`when` loop) b

data Command = Compile CompileForm
             | PPrint String
             | Type String
             | Reload
             | Browse
             | Quit
             | Help
             | Noop

data CompileForm = CompileInteractive  String
                 | CompileFile         String

data InteractiveCommand = Cmd [String] String (String -> Command) String

commands :: [InteractiveCommand]
commands
  =  [ Cmd [":browse"]      ""        (const Browse) "Ver los nombres en scope",
       Cmd [":load"]        "<file>"  (Compile . CompileFile)
                                                     "Cargar un programa desde un archivo",
       Cmd [":print"]       "<exp>"   PPrint          "Imprime un término y sus ASTs sin evaluarlo",
       Cmd [":reload"]      ""        (const Reload)         "Vuelve a cargar el último archivo cargado",
       Cmd [":type"]        "<exp>"   Type           "Chequea el tipo de una expresión",
       Cmd [":quit",":Q"]        ""        (const Quit)   "Salir del intérprete",
       Cmd [":help",":?"]   ""        (const Help)   "Mostrar esta lista de comandos" ]

helpTxt :: [InteractiveCommand] -> String
helpTxt cs
  =  "Lista de comandos:  Cualquier comando puede ser abreviado a :c donde\n" ++
     "c es el primer caracter del nombre completo.\n\n" ++
     "<expr>                  evaluar la expresión\n" ++
     "let <var> = <expr>      definir una variable\n" ++
     unlines (map (\ (Cmd c a _ d) ->
                   let  ct = concat (intersperse ", " (map (++ if null a then "" else " " ++ a) c))
                   in   ct ++ replicate ((24 - length ct) `max` 2) ' ' ++ d) cs)

-- | Parser simple de comando interactivos
interpretCommand :: String -> IO Command
interpretCommand x
  =  if isPrefixOf ":" x then
       do  let  (cmd,t')  =  break isSpace x
                t         =  dropWhile isSpace t'
           --  find matching commands
           let  matching  =  filter (\ (Cmd cs _ _ _) -> any (isPrefixOf cmd) cs) commands
           case matching of
             []  ->  do  putStrLn ("Comando desconocido `" ++ cmd ++ "'. Escriba :? para recibir ayuda.")
                         return Noop
             [Cmd _ _ f _]
                 ->  do  return (f t)
             _   ->  do  putStrLn ("Comando ambigüo, podría ser " ++
                                   concat (intersperse ", " [ head cs | Cmd cs _ _ _ <- matching ]) ++ ".")
                         return Noop

     else
       return (Compile (CompileInteractive x))

-- | 'handleCommand' interpreta un comando y devuelve un booleano
-- indicando si se debe salir del programa o no.
handleCommand ::  MonadFD4 m => Command -> Mode -> m Bool
handleCommand cmd mode = do
   s@GlEnv {..} <- get
   case cmd of
       Quit   ->  return False
       Noop   ->  return True
       Help   ->  printFD4 (helpTxt commands) >> return True
       Browse ->  do  printFD4 (unlines [ name | name <- reverse (nub (map declName glb)) ])
                      return True
       Compile c ->
                  do  case c of
                          CompileInteractive e -> compilePhrase e mode
                          CompileFile f        -> put (s {lfile=f, cantDecl=0}) >> compileFile f mode
                      return True
       Reload ->  eraseLastFileDecls >> (getLastFile >>= (\f -> compileFile f mode)) >> return True
       PPrint e   -> printPhrase e >> return True
       Type e    -> typeCheckPhrase e >> return True

parseIO ::  MonadFD4 m => String -> P a -> String -> m a
parseIO filename p x = case runP p x filename of
                  Left e  -> throwError (ParseErr e)
                  Right r -> return r

compilePhrase ::  MonadFD4 m => String -> Mode -> m ()
compilePhrase x mode =
  do
    dot <- parseIO "<interactive>" declOrTm x
    case dot of 
      Left d  -> handleDecl d mode
      Right t -> handleTerm t mode

handleTerm ::  MonadFD4 m => SNTerm -> Mode -> m ()
handleTerm t m = case m of
                    Interactive -> handleTermBigS t
                    InteractiveCEK -> handleTermCEK t
                    _ -> failFD4 "Modo inválido"

handleTermBigS ::  MonadFD4 m => SNTerm -> m ()
handleTermBigS t = do  t' <- desugar t
                       tt <- elab t'
                       s <- get
                       ty <- tc tt (tyEnv s)
                       optt <- optimize tt
                       te <- eval optt
                       ppte <- pp te
                       printFD4 (ppte ++ " : " ++ ppTy ty)

handleTermCEK :: MonadFD4 m => SNTerm -> m ()
handleTermCEK t = do t' <- desugar t
                     tt <- elab t'
                     s <- get
                     ty <- tc tt (tyEnv s)
                     optt <- optimize tt
                     v <- search optt [] []
                     te <- val2Term v
                     ppte <- pp te
                     printFD4 (ppte ++ " : " ++ ppTy ty)

printPhrase   :: MonadFD4 m => String -> m ()
printPhrase x =
  do
    x'' <- parseIO "<interactive>" tm x
    x' <- desugar x''
    ex <- elab x'
    t  <- case x' of 
           (V p f) -> maybe ex id <$> lookupDecl f
           _       -> return ex  
    printFD4 "SNTerm:"
    printFD4 (show x'')
    printFD4 "\nNTerm:"
    printFD4 (show x')
    printFD4 "\nTerm:"
    printFD4 (show t)

typeCheckPhrase :: MonadFD4 m => String -> m ()
typeCheckPhrase x = do
         t <- parseIO "<interactive>" tm x
         t' <- desugar t
         tt <- elab t'
         s <- get
         ty <- tc tt (tyEnv s)
         printFD4 (ppTy ty)

loadFile ::  MonadFD4 m => FilePath -> m [SDecl]
loadFile f = do
    let filename = reverse(dropWhile isSpace (reverse f))
    x <- liftIO $ catch (readFile filename)
               (\e -> do let err = show (e :: IOException)
                         hPutStrLn stderr ("No se pudo abrir el archivo " ++ filename ++ ": " ++ err)
                         return "")
    setLastFile filename
    parseIO filename program x

compileFiles ::  MonadFD4 m => [FilePath] -> Mode -> m ()
compileFiles [] _        = return ()
compileFiles (x:xs) mode = do
        modify (\s -> s { lfile = x, inter = False })
        compileFile x mode
        compileFiles xs mode

compileFile ::  MonadFD4 m => FilePath -> Mode -> m ()
compileFile f mode = do
    decls <- loadFile f
    mapM_ (\d -> handleDecl d mode) decls

typecheckFile ::  MonadFD4 m => FilePath -> m ()
typecheckFile f = do
    printFD4  ("Chequeando " ++ f)
    decls <- loadFile f
    ppterms <- go decls
    mapM_ printFD4 ppterms
    where
      go :: MonadFD4 m => [SDecl] -> m [String]
      go [] = return []
      go ((DeclSTy _ n t) : ds) = do ty <- desugarTy t
                                     addSTy n ty
                                     go ds
      go (decl : ds) = do decl' <- desugarDecl >=> typecheckDecl $ decl
                          opDecl <- optimizeDecl decl' 
                          ppOpDecl <- ppDecl opDecl
                          ds' <- go ds
                          return $ ppOpDecl : ds'

typecheckDecl :: MonadFD4 m => Decl NTerm -> m (Decl Term)
typecheckDecl (Decl p x ty t) = do
        tt <- elab t
        let dd = (Decl p x ty tt)
        tcDecl dd
        return (Decl p x ty tt)

handleDecl ::  MonadFD4 m => SDecl -> Mode -> m ()
handleDecl (DeclSTy _ n t) _ = do ty <- desugarTy t
                                  addSTy n ty
handleDecl d m = case m of
                    Interactive -> handleDeclBigS d
                    InteractiveCEK -> handleDeclCEK d
                    _ -> failFD4 "Modo inválido"

handleDeclBigS ::  MonadFD4 m => SDecl -> m ()
handleDeclBigS d = do decl <- desugarDecl d
                      decl' <- typecheckDecl decl
                      (Decl p x ty tt) <- optimizeDecl decl'
                      te <- eval tt
                      addDecl (Decl p x ty te)

handleDeclCEK :: MonadFD4 m => SDecl -> m ()
handleDeclCEK d = do decl <- desugarDecl d
                     decl' <- typecheckDecl decl
                     (Decl p x ty tt) <- optimizeDecl decl'
                     v <- search tt [] [] 
                     te <- val2Term v 
                     addDecl (Decl p x ty te)

-- Puede tener 2 puntos? Sí
bytecompileFile :: MonadFD4 m => FilePath -> m ()
bytecompileFile f = do -- printFD4 ("Abriendo "++f++"...")
                       let (name, ext) = splitExtension f
                       if ext /= ".fd4"
                       then failFD4 "Error al abrir el código fuente: extensión inválida."
                       else do sdecls <- loadFile f
                               decls <- go sdecls
                               -- printFD4 "Compilando a BVM..."
                               bc <- bytecompileModule decls
                               -- printFD4 "Escribiendo archivo..."
                               liftIO $ bcWrite bc $ name ++ ".byte"
                               -- printFD4 "Archivo compilado"                    
                    where go [] = return []
                          go (sd:xs) = case sd of
                                        (DeclSTy _ n t) -> do ty <- desugarTy t
                                                              addSTy n ty
                                                              go xs
                                        _ -> do decl <- desugarDecl sd
                                                (Decl p x ty tt) <- typecheckDecl decl
                                                optt <- optimize tt
                                                let dd = (Decl p x ty optt)
                                                xs' <- go xs
                                                return (dd : xs')

bytecodeRun :: MonadFD4 m => FilePath -> m ()
bytecodeRun f = do printFD4 ("Abriendo "++f++"...")
                   let (_, ext) = splitExtension f
                   if ext /= ".byte"
                   then failFD4 "Error al abrir el ejecutable: extensión inválida."
                   else do bc <- liftIO $ bcRead f
                           printFD4("Corriendo programa")
                           runBC bc

ccFile :: MonadFD4 m => FilePath -> m ()
ccFile f = do -- printFD4 ("Abriendo "++f++"...")
              let (name, ext) = splitExtension f
              if ext /= ".fd4"
              then failFD4 "Error al abrir el código fuente: extensión inválida."
              else do sdecls <- loadFile f
                      decls <- go sdecls
                      -- printFD4 "Compilando a C..."
                      let cc = (ir2C . runCC) decls
                      -- printFD4 "Escribiendo archivo..."
                      liftIO $ writeFile (name ++ ".c") cc
                      -- printFD4 "Archivo compilado"                    
              where go [] = return []
                    go (sd:xs) = case sd of
                                  (DeclSTy _ n t) -> do ty <- desugarTy t
                                                        addSTy n ty
                                                        go xs
                                  _ -> do decl <- desugarDecl sd
                                          (Decl p x ty tt) <- typecheckDecl decl
                                          optt <- optimize tt
                                          let dd = (Decl p x ty optt)
                                          xs' <- go xs
                                          return (dd : xs')

optimizeDecl :: MonadFD4 m => Decl Term -> m (Decl Term)
optimizeDecl (Decl p x ty t) = do optt <- optimize t
                                  return (Decl p x ty optt)
