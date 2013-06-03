module Language.QuickSilver.Generate.LLVM.Types 
    (
     -- Type, L.TypeKind(..),
     -- structCreateNamed, structSetBody,

     -- typeOfVal,

     -- structType, countStructElementTypes,

     -- getTypeKind, getElementType, getTypeByName,

     -- int1TypeM, int8TypeM, int16TypeM,
     -- int32TypeM, int64TypeM, doubleTypeM, voidTypeM,

     -- pointer0, W.pointerType, arrayType
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
