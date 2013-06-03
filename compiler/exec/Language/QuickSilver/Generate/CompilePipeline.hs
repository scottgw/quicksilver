module Language.QuickSilver.Generate.CompilePipeline (compileIO, generateModule, compileToObject) where

import Control.Lens
import Control.Monad
import Control.Monad.Reader

import Data.Generics
import Data.Text as Text

-- import GHC.Constants

import Language.QuickSilver.Syntax
import Language.QuickSilver.TypeCheck.TypedExpr as T
import Language.QuickSilver.Generate.Clas
import Language.QuickSilver.Generate.LLVM.Simple
import Language.QuickSilver.Generate.LLVM.Build

import LLVM.Wrapper.BitWriter

type Compile a = ReaderT TClass IO a

compileToObject outFile genMain clas = 
    runReaderT (ask >>= lift . generateObject outFile genMain) clas

compileIO :: Bool -> String -> Bool -> TClass -> IO ()
compileIO debugFlag outfile = runReaderT . compile debugFlag outfile

compile :: Bool -> String -> Bool -> Compile ()
compile = generateLL

generateLL :: Bool -> String -> Bool -> Compile ()
generateLL debugFlag outFile genMain = 
  ask >>= lift . generate debugFlag outFile genMain

-- dataLayout32 = "e-p:32:32:32-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:32:64-f32:32:32-f64:32:64-v64:64:64-v128:128:128-a0:0:64-f80:32:32-n8:16:32"
-- dataLayout64 = "e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64-S128"

-- targetTriple32 = "x86-pc-linux-gnu"
-- targetTriple64 = "x86_64-pc-linux-gnu"

-- -- "e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64"

-- dataLayout = if wORD_SIZE_IN_BITS == 32
--              then dataLayout32
--              else dataLayout64

-- targetTriple = if wORD_SIZE_IN_BITS == 32
--               then targetTriple32
--               else targetTriple64


setLayoutAndTriple _modul = return ()
    -- do withCString dataLayout (setDataLayout modul)
    --    withCString targetTriple (setTarget modul)

generateObject :: Bool -> String -> Bool -> TClass -> IO ()
generateObject debugFlag _outFile genMain clas = do
  when debugFlag $ putStrLn "Compiling to object code"
  -- passMan <- createPassManager
  modul <- generateModule debugFlag genMain clas

  setLayoutAndTriple modul

  -- withCString outFile (addEmitObjectPass modul)
  -- runPassManager passMan modul

  return ()

generate :: Bool -> String -> Bool -> TClass -> IO ()
generate debugFlag outFile genMain clas = do
  when debugFlag $ putStrLn "Compiling to bytecode"
  modul <- generateModule debugFlag genMain clas

  setLayoutAndTriple modul

  _ <- writeBitcodeToFile modul outFile
  return ()

generateModule :: Bool -> Bool -> TClass -> IO Module
generateModule debugFlag genMain clas =
  runBuild 
    debugFlag 
    (view className clas) 
    (extractStrings clas >> genClass clas genMain >> askModule)

extractStrings :: TClass -> Build ()
extractStrings cls = everywhereM (mkM extract) cls >> return ()
    where
      extract e@(T.LitString str) = string (Text.unpack str) >> return e
      extract e = return e
