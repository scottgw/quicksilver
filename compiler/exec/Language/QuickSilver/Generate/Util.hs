{-# LANGUAGE OverloadedStrings #-}
module Language.QuickSilver.Generate.Util where

import Language.QuickSilver.Generate.LLVM.Simple
import Language.QuickSilver.Generate.LLVM.Build

mallocSz :: Value -> Build Value
-- mallocSz = call "GC_malloc" . (:[])
mallocSz = call' "qs_malloc" . (:[])

mallocTyp :: Type -> Build Value
mallocTyp t = do
    sz <- sizeOf t
    mallocSz sz

sizeOf :: Type -> Build Value
sizeOf t = do
  let pt = pointer0 t
  one <- int 1 
  ptr <- gep (nul pt) [one]
  i64 <- int64TypeM
  ptrToInt ptr i64 "convert pointer to size"

arrayMalloc :: Value -> Type -> Build Value
arrayMalloc i t = do
  st <- sizeOf t
  arSize <- mul st i "multiply number of elements"
  ptr <- mallocSz arSize
  bitcast ptr (pointer0 t) "casting ptr to array"

