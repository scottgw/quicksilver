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
import Language.QuickSilver.Generate.LLVM.Build

attributeIndex :: ClasInterface -> Text -> Maybe Int
attributeIndex c attrName =
    findIndex ( (== attrName) . declName . attrDecl) (view attributes c)

getAttribute :: ClasInterface -> Text -> Value -> Build Value
getAttribute c attrName obj = do
  let Just offset = attributeIndex c attrName
  gepInt obj [0, offset]

mallocSeparate :: Typ -> Build Value
mallocSeparate t = do
  sepTyp <- typeOfM (Sep Nothing [] t)
  elemType <- getElementType sepTyp
  inst <- mallocTyp elemType
  bitcast inst sepTyp "casting char ptr to sep struct ptr"

mallocObject :: ClassName -> Build Value
mallocObject c = do
  clasTyp <- typeOfM (ClassType c [])
  kind <- getTypeKind clasTyp
  t <- case kind of
    PointerTypeKind -> getElementType clasTyp
    _ -> return clasTyp
  inst <- mallocTyp t
  bitcast inst clasTyp "casting char ptr to typed ptr"
