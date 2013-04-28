module Language.QuickSilver.Generate.Memory.Class 
    ( mallocClas
    , getAttribute
    , attributeOffset
    , clasSize
    , getVTableRef
    , getVTable
    -- construction
    , vtableFunc
    , mkClasTable
    , unClasTable
    , unClasRef
    , deepOffset
    ) where

import Control.Monad

import Data.List (findIndex)

import Language.QuickSilver.Syntax
import Language.QuickSilver.Util

import Language.QuickSilver.Generate.Memory.Attribute
import Language.QuickSilver.Generate.Memory.Feature
import Language.QuickSilver.Generate.Util

import Language.QuickSilver.Generate.LLVM.Simple
import Language.QuickSilver.Generate.LLVM.Util
import Language.QuickSilver.Generate.LLVM.Values
import Language.QuickSilver.Generate.LLVM.Types

newtype ClasTable = ClasTable {unClasTable :: [TypeRef]}

typToTable :: Typ -> Build ClasTable
typToTable = mkClasTable . rtClass <=< lookupClassEnv . classNameType

mkInheritTable :: [InheritClause] -> Build [ClasTable]
mkInheritTable = mapM (typToTable . inheritClass)
-- this should in fact LOOKUP the clastype, not reproduce it.

mkClasTable :: ClasInterface -> Build ClasTable
mkClasTable c = do
  funcs    <- mkFuncTable c
  inherits <- mkInheritTable (allInherited c)
  attrs    <- mkAttrTable c
  return $ mergeTables funcs inherits attrs

mergeTables :: FuncTableType -> [ClasTable] -> AttrTable -> ClasTable
mergeTables (FuncTableType ft) its (AttrTable at) =
    ClasTable  (concat [concatMap unClasTable its
                       ,at
                       ,[pointer0 ft]
                       ]
               )

newtype ClasRef = ClasRef { unClasRef :: ValueRef }

-- number of indices the class occupies in a `flat' view.
-- cycles would be bad here...
clasSize :: AbsClas body exp -> Build Int
clasSize c = do
  fmap ( + (numAttributes c + 1) -- for the vtable
       ) (inheritedSize c)

type AttrName = String

attributeIndex :: ClasInterface -> AttrName -> Maybe Int
attributeIndex c attrName =
    findIndex ( (== attrName) . declName) (allAttributeDecls c)

attributeOffset :: ClasInterface -> String -> Build (Maybe Int)
attributeOffset c attrName = do
  base <- inheritedSize c
  let offset = fmap (+ base) (attributeIndex c attrName)
  return offset

getAttribute :: ClasInterface -> String -> ValueRef -> Build ValueRef
getAttribute c attrName obj = do
  Just offset <- attributeOffset  c attrName
  zero <- int 0
  off  <- int offset
  gep obj [zero, off]

inheritedSize :: AbsClas body exp -> Build Int
inheritedSize = 
    fmap sum . mapM (clasSize <=< lookupClas . classNameType . inheritClass) . allInherited

numAttributes :: AbsClas body exp -> Int
numAttributes = length . allAttributes

deepOffset :: ClasInterface -> String -> Build (Maybe Int)
deepOffset c attrName =
  liftM2 mplus (attributeOffset c attrName)
               (do
                 supers <- lookupInherit c
                 ps     <- mapM (flip deepOffset attrName) supers
                 return (msum ps)
               )

mallocClas :: ClassName -> Build ClasRef
mallocClas c = do
  clasTyp <- lookupClasLType c
  inst    <- mallocTyp clasTyp

  initializeVTable c inst
  lookupClas c >>= initializeParents inst

  return $ ClasRef inst

initializeParents inst clasTyp = 
    case map inheritClass (allInherited clasTyp) of
      [] -> return ()
      [ClassType parent _] -> castAndInit parent inst
      _ -> error "initializeParents: more than one parent"

castAndInit clasName inst = do
  lType <- lookupClasLType clasName
  castedInst <- bitcast inst (pointer0 lType) 
                        ("Casting to parent " ++ clasName)
  initializeVTable clasName castedInst

getVTableRef clasName object = do
  size    <- clasSize =<< lookupClas clasName
  zero <- int 0
  lessSz <- int (size - 1)
  gep object [zero, lessSz]

getVTable clasName object = getVTableRef clasName object >>= flip load ""

initializeVTable clasName object = do
  vtable     <- lookupVTable clasName
  vtableRef  <- getVTableRef clasName object

  vtt <- typeOfVal vtable 
  hack <- bitcast vtableRef (pointer0 vtt) "hack"

  _ <- store vtable hack -- vtableRef
  return ()

vtableFunc :: ClasInterface -> ValueRef -> String -> Build ValueRef
vtableFunc c objRef featName = do
  let
    allFeats :: [RoutineI]
    allFeats = allFeatures c
    Just featureIdx = findIndex ((== featName) . featureName) allFeats

  vtable <- getVTable (className c) objRef >>= flip load ""
  debug (show (featName, featureIdx))
  extractValue vtable featureIdx
