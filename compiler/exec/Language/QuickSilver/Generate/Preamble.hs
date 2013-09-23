{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.QuickSilver.Generate.Preamble where

import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.Reader
    
import Data.HashMap.Strict (union, fromList)
import Data.Text (Text)

import Language.QuickSilver.Syntax
import Language.QuickSilver.Util

import Language.QuickSilver.Generate.DepGen
import Language.QuickSilver.Generate.Memory.Declarations
import Language.QuickSilver.Generate.LibQs
import Language.QuickSilver.Generate.LLVM.Simple
import Language.QuickSilver.Generate.LLVM.Build

import Language.QuickSilver.TypeCheck.TypedExpr

preamble :: TClass -> Build (BuildState -> BuildState)
preamble clas = do
  vt <- vtables
  debug "Declaring Qs functions"
  declareQsFuncs
  debug "Adding consts and string consts"
  declMap <- (liftM2 union) addConstDecls addStringConsts
  let declTrans = updEnv (union declMap)
  return (declTrans .  env vt)
      where
        vtables = (liftIO . depGenInt . view className) clas >>=
                  either (error . show)  genClassStructs 
        env classEnv =
          setCurrent (clasInterface $ untype clas) . setClassEnv classEnv

ptr :: Build Type
ptr = pointer0 <$> int8TypeM

constDecls :: [(Text, Build Type)]
constDecls = 
    [ ("llvm.sqrt.f64", funcType' doubleType [doubleType])
    , ("GC_init",       funcType' voidType [])   
    , ("GC_malloc",     funcType' ptr [int64TypeM])
    , ("qs_init",     funcType' ptr [])
    , ("qs_malloc",     funcType' ptr [int64TypeM])
    , ("malloc",     funcType' ptr [int64TypeM])
    , ("__cxa_throw",   cxaThrowType)
    , ("__cxa_allocate_exception", 
                        funcType' ptr [int32TypeM])
    , ("__cxa_begin_catch", funcType' ptr [ptr])
    , ("__cxa_end_catch", funcType' voidType [])

    , ("__gxx_personality_v0", funcTypeVar' int32TypeM [])

    , ("llvm.eh.exception", funcType' ptr [])
    , ("llvm.eh.selector.i64",  funcTypeVar' int64TypeM [ptr, ptr])
    , ("llvm.eh.typeid.for", funcType' int32TypeM [ptr])
    ]

addConstDecls :: Build (Map Text Value)
addConstDecls = fromList `fmap` mapM addDecl constDecls
    where
      addDecl (str, t) = (str,) <$> (addFunction str =<< t)

stringConsts :: [(Text, Build Value, Linkage)]
stringConsts = 
    [("_ZTIi", stdTypeInfoExtern, ExternalLinkage)
    ]

addStringConsts :: Build (Map Text Value)
addStringConsts = fromList `fmap` mapM addConst stringConsts
    where addConst (str, vr, l) = do
            v <- vr
            setLinkage v l
            return (str, v)

cxaThrowType :: Build Type
cxaThrowType = do
  -- t <- stdTypeInfoType
  destr <- funcType' voidType [ptr]
  funcType' voidType [ptr, ptr, return $ pointer0 destr]

stdTypeInfoType :: Build Type
stdTypeInfoType = do
  ptrT <- ptr
  str <- structType [ptrT, ptrT] False
  structType [str] False

stdTypeInfoExtern :: Build Value
stdTypeInfoExtern = do
  g <- flip addGlobal "_ZTIi" =<< ptr
  setGlobalConstant g True
  return g
