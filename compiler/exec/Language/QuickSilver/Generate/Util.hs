{-# LANGUAGE OverloadedStrings #-}
module Language.QuickSilver.Generate.Util where

import Data.Text
import Language.QuickSilver.Generate.LLVM.Simple
import Language.QuickSilver.Generate.LLVM.Types
import Language.QuickSilver.Generate.LLVM.Values

mallocSz :: ValueRef -> Build ValueRef
mallocSz = call "GC_malloc" . (:[])

mallocTyp :: TypeRef -> Build ValueRef
mallocTyp t = do
    sz <- sizeOf t
    pt <- mallocSz sz
    bitcast pt t "casting char ptr to typed ptr"

sizeOf :: TypeRef -> Build ValueRef
sizeOf t = do
  let pt = pointer0 t
  one <- int 1 
  ptr <- gep (nul pt) [one]
  i32 <- int32TypeM
  ptrToInt ptr i32 "convert pointer to size"

arrayMalloc :: ValueRef -> TypeRef -> Build ValueRef
arrayMalloc i t = do
  st <- sizeOf t
  arSize <- mul st i "multiply number of elements"
  ptr <- mallocSz arSize
  bitcast ptr (pointer0 t) "casting ptr to array"

