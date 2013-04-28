{-# LANGUAGE BangPatterns #-}

module Language.QuickSilver.Generate.Memory.Feature 
    ( FuncTableType (..)
    , mkFuncTable
    , featDeclType
    , structType') where

import Control.Monad.Reader

import Language.QuickSilver.Syntax
import Language.QuickSilver.Util

import Language.QuickSilver.Generate.LLVM.Simple
import Language.QuickSilver.Generate.LLVM.Types

import Language.QuickSilver.Generate.Memory.Attribute


{-

The runtime memory representation of an object should be as follows:

[pointer to function table,
 pointer to class lookup 
 inherited class data 1
 pointer to class lookup 
 inherited class data 2
 ...
 attribute 1
 attribute 2
 ...
]

The function table should contain only the functions
defined for the top-level class.
Redefined functions should be updated in the inherited
data.
This is important for upcasting the class to its parents
and still retaining the redefined features.

Downcasting probably requires unique identifiers for the classes,
and modifying each parent data section to include a pointer
that will be able to look this up.

-}

newtype FuncTableType = FuncTableType {unFuncTable :: TypeRef}

featDeclType :: RoutineI -> Build TypeRef
featDeclType f = join $ (liftM2 funcType) (featResTyp f) (featArgTyps f)

featResTyp :: RoutineI -> Build TypeRef
featResTyp = typeOfM . featureResult

featArgTyps :: RoutineI -> Build [TypeRef]
featArgTyps = mapM typeOfDecl . featureArgs

structType' :: [TypeRef] -> Build TypeRef
structType' = flip structType False

mkFuncTable :: ClasInterface -> Build FuncTableType
mkFuncTable = return . FuncTableType  <=< structType' <=<
              mapM (fmap pointer0 . featDeclType) . allFeatures