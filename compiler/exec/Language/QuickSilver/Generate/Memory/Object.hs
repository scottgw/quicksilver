{-# LANGUAGE OverloadedStrings #-}
module Language.QuickSilver.Generate.Memory.Object
    ( mallocObject
    , mallocSeparate
    , getAttribute
    , attributeIndex
    ) where

import Control.Lens

import Data.List (findIndex)
import Data.Text (Text)

import Language.QuickSilver.Syntax
import Language.QuickSilver.Generate.Memory.Types
import Language.QuickSilver.Generate.Util
import Language.QuickSilver.Generate.LLVM.Simple
import Language.QuickSilver.Generate.LLVM.Types
import Language.QuickSilver.Generate.LLVM.Values

attributeIndex :: ClasInterface -> Text -> Maybe Int
attributeIndex c attrName =
    findIndex ( (== attrName) . declName . attrDecl) (view attributes c)

getAttribute :: ClasInterface -> Text -> ValueRef -> Build ValueRef
getAttribute c attrName obj = do
  let Just offset = attributeIndex c attrName
  zero <- int 0
  off  <- int offset
  gep obj [zero, off]


mallocSeparate :: Typ -> Build ValueRef
mallocSeparate t = do
  sepTyp <- typeOfM (Sep Nothing [] t)
  t <- getElementType sepTyp
  inst <- mallocTyp t
  bitcast inst sepTyp "casting char ptr to sep struct ptr"


mallocObject :: ClassName -> Build ValueRef
mallocObject c = do
  clasTyp <- typeOfM (ClassType c [])
  kind <- getTypeKind clasTyp
  t <- case kind of
    PointerTypeKind -> getElementType clasTyp
    _ -> return clasTyp
  inst <- mallocTyp t
  bitcast inst clasTyp "casting char ptr to typed ptr"
