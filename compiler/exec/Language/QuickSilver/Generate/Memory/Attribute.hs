module Language.QuickSilver.Generate.Memory.Attribute 
    (typeOf, 
     typeOfM, 
     typeOfDecl) 
    where

import Control.Applicative
import Control.Lens

import qualified Data.HashMap.Strict as Map

import Language.QuickSilver.Syntax
import Language.QuickSilver.Util

import Language.QuickSilver.Generate.LLVM.Types
import Language.QuickSilver.Generate.LLVM.Util

newtype AttrTable = AttrTable {unAttrTable :: [TypeRef]}

typeOfDecl :: Decl -> Build TypeRef
typeOfDecl = typeOfM . declType

typeOf :: ClassEnv -> Typ -> Build TypeRef
typeOf e t =
    case t of
      NoType -> voidTypeM
      IntType -> int32TypeM
      BoolType -> int1TypeM
      DoubleType -> doubleTypeM
      CharType -> int8TypeM
      ClassType s _ ->
          let cInfo = maybe (error $ "typeOf: couldn't find class " ++ show s ++ " " ++ show e) id
                      (Map.lookup s e)
          in return $ pointer0 (rtClassStruct cInfo)

typeOfM :: Typ -> Build TypeRef
typeOfM t = askClassEnv >>= \env -> typeOf env t
