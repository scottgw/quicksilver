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
import Language.QuickSilver.Generate.LLVM.Util
import Language.QuickSilver.Generate.LLVM.Types

import Language.QuickSilver.TypeCheck.TypedExpr

preamble :: TClass -> Build (BuildState -> BuildState)
preamble clas = do
  vt <- vtables
  declareQsFuncs
  debug "Adding consts and string consts"
  declMap <- (liftM2 union) addConstDecls addStringConsts
  let declTrans = updEnv (union declMap)
  return (declTrans .  env vt)
      where
        vtables = (lift . depGenInt . view className) clas >>=
                  either (error . show)  genClassStructs 
        env classEnv =
          setCurrent (clasInterface $ untype clas) . setClassEnv classEnv

ptr :: Build Type
ptr = pointerType <$> int8TypeM <*> pure 0

constDecls :: [(Text, Build Type)]
constDecls = 
    [ ("llvm.sqrt.f64", funcType' doubleTypeM [doubleTypeM])
    , ("GC_init",       funcType' voidTypeM [])   
    , ("GC_malloc",     funcType' ptr [int64TypeM])
    , ("qs_init",     funcType' ptr [])
    , ("qs_malloc",     funcType' ptr [int64TypeM])
    , ("malloc",     funcType' ptr [int64TypeM])
    , ("__cxa_throw",   cxaThrowType)
    , ("__cxa_allocate_exception", 
                        funcType' ptr [int32TypeM])
    , ("__cxa_begin_catch", funcType' ptr [ptr])
    , ("__cxa_end_catch", funcType' voidTypeM [])

    , ("__gxx_personality_v0", funcTypeVar' int32TypeM [])

    , ("llvm.eh.exception", funcType' ptr [])
    , ("llvm.eh.selector.i64",  funcTypeVar' int64TypeM [ptr, ptr])
    , ("llvm.eh.typeid.for", funcType' int32TypeM [ptr])
    ]

addConstDecls :: Build (Map Text Value)
addConstDecls = fromList `fmap` mapM addDecl constDecls
    where
      addDecl (str, t) = (str,) <$> (addFunc str =<< t)

stringConsts :: [(Text, Build Value, Int)]
stringConsts = 
    [("_ZTIi", stdTypeInfoExtern, 0)
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
  destr <- funcType' voidTypeM [ptr]
  funcType' voidTypeM [ptr, ptr, return $ pointerType destr 0]

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
