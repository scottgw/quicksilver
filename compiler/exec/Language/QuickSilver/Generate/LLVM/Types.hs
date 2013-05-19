module Language.QuickSilver.Generate.LLVM.Types 
    (
     TypeRef, L.TypeKind(..),
     structCreateNamed, structSetBody,

     typeOfVal,

     structType, countStructElementTypes,

     getTypeKind, getElementType,

     int1TypeM, int8TypeM, int32TypeM, int64TypeM, doubleTypeM, voidTypeM,

     pointer0, pointerType, arrayType
     -- int1Type, int8Type, int32Type, doubleType,
     -- voidType
    ) where

import           Data.Text (Text)
import qualified Data.Text as Text

import Foreign.C
import Foreign.Ptr

import qualified LLVM.FFI.Core as L
import LLVM.FFI.Core 
    (
     TypeRef, ValueRef, TypeKind,
     -- int1Type, int8Type, int32Type, doubleType,
     -- voidType,             
     pointerType
    )
import Language.QuickSilver.Generate.LLVM.Util

pointer0 :: TypeRef -> TypeRef
pointer0 = (`pointerType` 0)

typeOfVal :: ValueRef -> Build TypeRef
typeOfVal = liftBuild1 L.typeOf

structCreateNamed :: Text -> Build TypeRef
structCreateNamed str = do
  c <- askContext
  lift $ withCString (Text.unpack str) (\cstr -> L.structCreateNamed c cstr)

structSetBody :: TypeRef -> [TypeRef] -> Bool -> Build ()
structSetBody struct elems packed =
    let len = fromIntegral $ length elems
    in lift $ withPtrArray elems (\ptr -> L.structSetBody struct ptr len packed)

getTypeKind :: TypeRef -> Build TypeKind
getTypeKind = lift .  L.getTypeKind

getElementType :: TypeRef -> Build TypeRef
getElementType = lift . L.getElementType

structType :: [TypeRef] -> Bool -> Build TypeRef
structType ts packed =
  let 
      strct :: L.ContextRef -> Ptr TypeRef -> IO TypeRef
      strct cont arr = L.structTypeInContext
                             cont
                             arr 
                             (fromIntegral . length   $ ts)
                             packed
  in withContext0 (\c -> withPtrArray ts (strct c))

countStructElementTypes :: TypeRef -> Int
countStructElementTypes = fromIntegral . L.countStructElementTypes

-- int32TypeM = withContext0 L.int32TypeInContext

int1TypeM = withContext0 L.int1TypeInContext
int8TypeM = withContext0 L.int8TypeInContext
int32TypeM = withContext0 L.int32TypeInContext
int64TypeM = withContext0 L.int64TypeInContext
doubleTypeM = withContext0 L.doubleTypeInContext
voidTypeM = withContext0 L.voidTypeInContext


arrayType :: TypeRef -> Int -> TypeRef
arrayType t = L.arrayType t . fromIntegral
