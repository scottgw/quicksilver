module Language.QuickSilver.Generate.Memory.Attribute 
    (mkAttrTable,
     AttrTable (..),
     toLLVMType,
     typeOf, 
     typeOfM, 
     typeOfDecl) 
    where

import Control.Applicative

import Data.Map (lookup)

import Language.QuickSilver.Syntax
import Language.QuickSilver.Util

import Language.QuickSilver.Generate.LLVM.Types
import Language.QuickSilver.Generate.LLVM.Util

newtype AttrTable = AttrTable {unAttrTable :: [TypeRef]}

mkAttrTable :: ClasInterface -> Build AttrTable
mkAttrTable ci = AttrTable <$> mapM typeOfDecl (allAttributeDecls ci)

typeOfDecl :: Decl -> Build TypeRef
typeOfDecl = typeOfM . declType

typeOf :: ClassEnv -> Typ -> Build TypeRef
typeOf e (ClassType "ARRAY" _) = pointer0 <$> typeOf e (ClassType "G" [])
typeOf e (ClassType "ARRAY_REAL" _) = pointer0 <$> typeOf e realType
typeOf _e (ClassType "CHARACTER_8_ARRAY" _) = pointer0 <$> int8TypeM
typeOf _e (ClassType "POINTER" _) = pointer0 <$> int8TypeM
typeOf e t@(ClassType s _) = 
    let cInfo = maybe (error $ "couldn't find class " ++ s ++ " " ++ show e) id
                (Data.Map.lookup s e)
    in if isBasic t
          then toLLVMType t
          else return $ pointer0 (rtClassStruct cInfo)
typeOf _e NoType = voidTypeM

typeOfM :: Typ -> Build TypeRef
typeOfM t = askClassEnv >>= \env -> typeOf env t

toLLVMType :: Typ -> Build TypeRef
toLLVMType NoType = voidTypeM
toLLVMType t
  | isBasic t = llvmTypeFromStr (classNameType t)
  | otherwise = error "toLLVMType: this is for primitive types only"
  where 
    llvmTypeFromStr str =
      case str of
        "REAL_64"    -> doubleTypeM
        "INTEGER_32" -> int32TypeM
        "CHARACTER_8" -> int8TypeM
        "BOOLEAN"    -> int1TypeM
