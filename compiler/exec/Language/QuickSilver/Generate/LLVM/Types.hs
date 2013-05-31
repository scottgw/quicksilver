module Language.QuickSilver.Generate.LLVM.Types 
    (
     Type, L.TypeKind(..),
     structCreateNamed, structSetBody,

     typeOfVal,

     structType, countStructElementTypes,

     getTypeKind, getElementType, getTypeByName,

     int1TypeM, int8TypeM, int16TypeM,
     int32TypeM, int64TypeM, doubleTypeM, voidTypeM,

     pointer0, W.pointerType, arrayType
     -- int1Type, int8Type, int32Type, doubleType,
     -- voidType
    ) where

import           Data.Text (Text)
import qualified Data.Text as Text

import Foreign.C
import Foreign.Ptr

import qualified LLVM.Wrapper.Core as W 
import           LLVM.Wrapper.Core ( Type, Value, BasicBlock, TypeKind
                                   , Builder, Context, Module
                                   , CallingConvention, FPPredicate
                                   , IntPredicate)

import qualified LLVM.FFI.Core as L
-- import LLVM.FFI.Core 
--     (
--      TypeRef, ValueRef, TypeKind,
--      -- int1Type, int8Type, int32Type, doubleType,
--      -- voidType,             
--      pointerType
--     )
import Language.QuickSilver.Generate.LLVM.Util

pointer0 :: Type -> Type
pointer0 = (`W.pointerType` 0)

typeOfVal :: Value -> Build Type
typeOfVal = liftBuild1 W.typeOf

structCreateNamed :: Text -> Build Type
structCreateNamed str = do
  c <- askContext
  lift $ W.structCreateNamedInContext c (Text.unpack str)

structSetBody :: Type -> [Type] -> Bool -> Build ()
structSetBody struct body pack = lift (W.structSetBody struct body pack)

getTypeKind :: Type -> Build TypeKind
getTypeKind = lift . W.getTypeKind

getTypeByName :: Text -> Build (Maybe Type)
getTypeByName name = 
    do modul <- askModule
       lift (W.getTypeByName modul (Text.unpack name))

getElementType :: Type -> Build Type
getElementType t = lift (L.getElementType t)

structType :: [Type] -> Bool -> Build Type
structType = withContext2 W.structTypeInContext

countStructElementTypes :: Type -> Int
countStructElementTypes = fromIntegral . L.countStructElementTypes

-- int32TypeM = withContext0 L.int32TypeInContext

int1TypeM = withContext1 W.intTypeInContext 1
int8TypeM = withContext1 W.intTypeInContext 8
int16TypeM = withContext1 W.intTypeInContext 16
int32TypeM = withContext1 W.intTypeInContext 32
int64TypeM = withContext1 W.intTypeInContext 64
doubleTypeM = withContext0 W.doubleTypeInContext
voidTypeM = withContext0 W.voidTypeInContext


arrayType :: Type -> Int -> Type
arrayType t int  = W.arrayType t (fromIntegral int)
