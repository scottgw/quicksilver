{-# LANGUAGE OverloadedStrings #-}
module Language.QuickSilver.Generate.Memory.Types
    (typeOf, 
     typeOfM, 
     typeOfDecl,
     isSpecialClass,
     isSpecialClassName,
     mkSpecialClassType,
     featDeclType
     ) 
    where

import           Control.Applicative
import           Control.Lens
import           Control.Monad

import           Foreign.Ptr

import           Data.Text (Text)
import qualified Data.Text as Text

import qualified Data.HashMap.Strict as Map

import           Language.QuickSilver.Syntax
import           Language.QuickSilver.Generate.LibQs
import           Language.QuickSilver.Generate.LLVM.Simple
import           Language.QuickSilver.Generate.LLVM.Types
import           Language.QuickSilver.Generate.LLVM.Util

typeOfDecl :: Decl -> Build Type
typeOfDecl = typeOfM . declType

typeOf :: ClassEnv -> Typ -> Build Type
typeOf e t =
    case t of
      NoType -> voidTypeM
      VoidType -> pointer0 <$> int8TypeM
      AnyIntType -> int64TypeM
      Int8Type -> int8TypeM
      Int16Type -> int16TypeM
      Int32Type -> int32TypeM
      Int64Type -> int64TypeM
      BoolType -> int1TypeM
      DoubleType -> doubleTypeM
      CharType -> int8TypeM
      AnyRefType _ -> pointer0 <$> int8TypeM
      ProcessorType -> procTypeM
      Sep _ _ s -> sepClass s
      ClassType s _ -> processClass s
    where
      sepClass s =
        do sepTypeMb <- getTypeByName "separate_wrapper"
           case sepTypeMb of
             Nothing ->
               do sepStruct <- structCreateNamed "separate_wrapper"
                  procType <- procTypeM
                  voidPtrRef <- voidPtrType
                  structSetBody sepStruct [procType, voidPtrRef] True
                  return (pointer0 sepStruct)
             Just sepType -> return (pointer0 sepType)

      processClass s =
        case Map.lookup s e of
          Just (ClassInfo cls typeValEi) ->
            if isSpecialClass cls
            then return (either id id typeValEi)
            else return (either pointer0 id typeValEi)
                 -- Regular class needs to add on the ptr
          Nothing -> err
        where err = error $ concat ["typeOf: couldn't find class " 
                             , show s ++ " " ++ show e]

        
typeOfM :: Typ -> Build Type
typeOfM t = askClassEnv >>= \env -> typeOf env t

isSpecialClass :: ClasInterface -> Bool
isSpecialClass cls = isSpecialClassName (view className cls)

isSpecialClassName :: Text -> Bool
isSpecialClassName name = name `elem` map fst nameAndType

nameAndType :: [(Text, Build Type)]
nameAndType =
    [("Pointer_8", pointer0 <$> int8TypeM)
    ,("Array", pointer0 <$> pointer0 <$> voidPtrType)
    ]

mkSpecialClassType :: Text -> Build Type
mkSpecialClassType name = 
    case lookup name nameAndType of
      Just t -> t
      Nothing ->
          error $ "mkSpecialClassType: non special type: " ++ Text.unpack name


featDeclType :: RoutineI -> Build Type
featDeclType f = join $ (liftM2 funcType) (featResTyp f) (featArgTyps f)

featResTyp :: RoutineI -> Build Type
featResTyp = typeOfM . routineResult

featArgTyps :: RoutineI -> Build [Type]
featArgTyps = mapM typeOfDecl . routineArgs
