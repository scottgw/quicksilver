{-# LANGUAGE BangPatterns #-}

module Language.QuickSilver.Generate.Memory.Feature 
    ( featDeclType
    , structType') where

import Control.Monad.Reader

import Language.QuickSilver.Syntax
import Language.QuickSilver.Util

import Language.QuickSilver.Generate.LLVM.Simple
import Language.QuickSilver.Generate.LLVM.Types

import Language.QuickSilver.Generate.Memory.Attribute

featDeclType :: RoutineI -> Build TypeRef
featDeclType f = join $ (liftM2 funcType) (featResTyp f) (featArgTyps f)

featResTyp :: RoutineI -> Build TypeRef
featResTyp = typeOfM . routineResult

featArgTyps :: RoutineI -> Build [TypeRef]
featArgTyps = mapM typeOfDecl . routineArgs

structType' :: [TypeRef] -> Build TypeRef
structType' = flip structType False
