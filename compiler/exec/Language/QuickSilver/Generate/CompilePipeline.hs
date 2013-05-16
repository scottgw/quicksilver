module Language.QuickSilver.Generate.CompilePipeline (compileIO, generateModule, compileToObject) where

import Control.Lens
import Control.Monad
import Control.Monad.Reader

import Foreign.C.String

import GHC.Constants

import Language.QuickSilver.Syntax

import Language.QuickSilver.TypeCheck.TypedExpr

import Language.QuickSilver.Generate.Clas
import Language.QuickSilver.Generate.ExtractStrings

import Language.QuickSilver.Generate.LLVM.Simple
import LLVM.FFI.Core
import LLVM.FFI.Support 

type Compile a = ReaderT TClass IO a

compileToObject outFile genMain clas = 
    runReaderT (ask >>= lift . generateObject outFile genMain) clas

compileIO :: Bool -> String -> Bool -> TClass -> IO ()
compileIO debug outfile = runReaderT . compile debug outfile

compile :: Bool -> String -> Bool -> Compile ()
compile = generateLL

generateLL :: Bool -> String -> Bool -> Compile ()
generateLL debug outFile genMain = 
  ask >>= lift . generate debug outFile genMain

dataLayout32 = "e-p:32:32:32-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:32:64-f32:32:32-f64:32:64-v64:64:64-v128:128:128-a0:0:64-f80:32:32-n8:16:32"
dataLayout64 = "e-p:64:64:64-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:64:64-f32:32:32-f64:64:64-v64:64:64-v128:128:128-a0:0:64-s0:64:64-f80:128:128-n8:16:32:64"

dataLayout = if wORD_SIZE_IN_BITS == 32
             then dataLayout32
             else dataLayout64


generateObject :: Bool -> String -> Bool -> TClass -> IO ()
generateObject debug outFile genMain clas = do
  when debug $ putStrLn "Compiling to object code"
  passMan <- createPassManager
  modul <- generateModule debug genMain clas
  withCString dataLayout (setDataLayout modul)

  withCString outFile (addEmitObjectPass modul)
  -- runPassManager passMan modul

  return ()

generate :: Bool -> String -> Bool -> TClass -> IO ()
generate debug outFile genMain clas = do
  when debug $ putStrLn "Compiling to bytecode"
  modul <- generateModule debug genMain clas
  withCString dataLayout (setDataLayout modul)
  _ <- writeModuleToFile modul outFile
  return ()

generateModule :: Bool -> Bool -> TClass -> IO ModuleRef
generateModule debug genMain clas =
  runBuild 
    debug 
    (view className clas) 
    (fromClass clas >> genClass clas genMain >> askModule)
