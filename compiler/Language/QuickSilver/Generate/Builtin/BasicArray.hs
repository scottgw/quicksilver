module Language.QuickSilver.Generate.Builtin.BasicArray where

import Data.Map

import Language.QuickSilver.Syntax (Typ(..))
import Language.QuickSilver.Util

import Language.QuickSilver.Generate.Eval
import Language.QuickSilver.Generate.Memory.Type
import Language.QuickSilver.Generate.Memory.Attribute
import Language.QuickSilver.Generate.Util

import Language.QuickSilver.Generate.LLVM.Simple

import Language.QuickSilver.TypeCheck.TypedExpr

data ArrayDescr = 
  ArrayDescr 
    { createArray :: Map (String, String) (Build ValueRef)
    , arrayFunctions :: Map (String, String) (Build ())
    }
    
mkBasicArray :: Typ -> ArrayDescr
mkBasicArray t = ArrayDescr createMap funcMap
  where
    arrayName = classNameType t ++ "_ARRAY"
    arrayType = ClassType arrayName []
    
    createMap = mapKeys ((,) arrayName) (fromList [("make", make_create)])
    funcMap   = mapKeys ((,) arrayName) (fromList functionList)

    functionList :: [(String, Build ())]
    functionList = [("make",make)
                   ,("item",item)
                   ,("put" ,put)
                   ]

    indexPtr :: Build ValueRef
    indexPtr = do
      a <- evalUnPos (CurrentVar arrayType) >>= flip load ""
      i <- evalUnPos (Var "i" intType) >>= flip load ""
      gep a [i]

    item :: Build ()
    item = do
      xPtr <- indexPtr
      x <- load xPtr "indexing array"
      s <- lookupEnv "Result"
      _ <- store x s
      ret x

    make :: Build ()
    make = retVoid >> return ()

    put :: Build ()
    put = do
      x <- evalUnPos (Var "x" t)
      xPtr <- indexPtr
      x' <- load x ""
      _ <- store x' xPtr 
      retVoid
      return ()

    make_create :: Build ValueRef
    make_create = do
      f <- getNamedFunction (featureAsCreate arrayName "make")
      let n = getParam f 0
      llvmType <- toLLVMType t
      arrayMalloc n llvmType
