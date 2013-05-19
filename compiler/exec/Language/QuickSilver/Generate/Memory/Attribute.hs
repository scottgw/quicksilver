{-# LANGUAGE OverloadedStrings #-}

module Language.QuickSilver.Generate.Memory.Attribute 
    (typeOf, 
     typeOfM, 
     typeOfDecl,
     isSpecialClass,
     mkSpecialClassType
     ) 
    where

import Control.Applicative
import Control.Lens

import           Data.Text (Text)
import qualified Data.Text as Text

import qualified Data.HashMap.Strict as Map

import Language.QuickSilver.Syntax
import Language.QuickSilver.Generate.LLVM.Types
import Language.QuickSilver.Generate.LLVM.Util

typeOfDecl :: Decl -> Build TypeRef
typeOfDecl = typeOfM . declType

typeOf :: ClassEnv -> Typ -> Build TypeRef
typeOf e t =
    case t of
      NoType -> voidTypeM
      VoidType -> pointer0 <$> int8TypeM
      AnyIntType -> int64TypeM
      Int8Type -> int8TypeM
      Int64Type -> int64TypeM
      BoolType -> int1TypeM
      DoubleType -> doubleTypeM
      CharType -> int8TypeM
      ClassType s _ ->
        case Map.lookup s e of
          Just (ClassInfo cls typeValEi) ->
            if isSpecialClass cls
            then return (either id id typeValEi)
            else return (either pointer0 id typeValEi)
                 -- Regular class needs to add on the ptr
          Nothing -> err
        where err = error $ concat ["typeOf: couldn't find class " 
                             , show s ++ " " ++ show e]

typeOfM :: Typ -> Build TypeRef
typeOfM t = askClassEnv >>= \env -> typeOf env t

isSpecialClass :: ClasInterface -> Bool
isSpecialClass cls = view className cls `elem` map fst nameAndType

nameAndType :: [(Text, Build TypeRef)]
nameAndType =
    [("Pointer_8", pointer0 <$> int8TypeM)
    ]

mkSpecialClassType :: Text -> Build TypeRef
mkSpecialClassType name = 
    case lookup name nameAndType of
      Just t -> t
      Nothing ->
          error $ "mkSpecialClassType: non special type: " ++ Text.unpack name
