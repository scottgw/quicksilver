{-# LANGUAGE OverloadedStrings #-}
module Language.QuickSilver.Generate.Memory.Object
    ( mallocObject
    , getAttribute
    , attributeIndex
    ) where

import Control.Applicative
import Control.Lens

import Data.List (findIndex)
import Data.Text (Text)

import Language.QuickSilver.Syntax
import Language.QuickSilver.Generate.Memory.Types
import Language.QuickSilver.Generate.Util
import Language.QuickSilver.Generate.LLVM.Simple
import Language.QuickSilver.Generate.LLVM.Types
import Language.QuickSilver.Generate.LLVM.Util
import Language.QuickSilver.Generate.LLVM.Values

-- number of indices the class occupies in a `flat' view.
-- cycles would be bad here...
objectSize :: AbsClas body exp -> Int
objectSize = numAttributes

attributeIndex :: ClasInterface -> Text -> Maybe Int
attributeIndex c attrName =
    findIndex ( (== attrName) . declName . attrDecl) (view attributes c)

getAttribute :: ClasInterface -> Text -> ValueRef -> Build ValueRef
getAttribute c attrName obj = do
  let Just offset = attributeIndex c attrName
  zero <- int 0
  off  <- int offset
  gep obj [zero, off]

numAttributes :: AbsClas body exp -> Int
numAttributes = length . view attributes

mallocObject :: ClassName -> Build ValueRef
mallocObject c = do
  clasTyp <- typeOfM (ClassType c [])
  kind <- getTypeKind clasTyp
  t <- case kind of
    PointerTypeKind -> getElementType clasTyp
    _ -> return clasTyp
  inst <- mallocTyp t
  bitcast inst clasTyp "casting char ptr to typed ptr"
