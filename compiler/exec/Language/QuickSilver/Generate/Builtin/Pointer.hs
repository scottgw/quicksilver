module Language.QuickSilver.Generate.Builtin.Pointer where

import Data.Map

import Language.QuickSilver.Syntax (Typ (..))
import Language.QuickSilver.Util
import Language.QuickSilver.TypeCheck.TypedExpr

import Language.QuickSilver.Generate.Eval

import qualified Language.QuickSilver.Generate.LLVM.Simple as L

featMap :: Map (String, String) (L.Build ())
featMap = mapKeys ((,) "INTEGER_32") (fromList featureList)

creatMap :: Map (String, String) (L.Build L.ValueRef)
creatMap = mapKeys ((,) "INTEGER_32") (fromList createList)

createList :: [(String, L.Build L.ValueRef)]
createList = []

featureList :: [(String, L.Build ())]
featureList = 
    [("add", add)
    ,("char_item", char_item)
    ]

add :: L.Build ()
add = do
  curr <- loadCurrent
  offset <- loadOffset
  res <- L.gep curr [offset]
  L.ret res

char_item :: L.Build ()
char_item = do
  curr <- loadCurrent
  res <- L.load curr "char_item"
  L.ret res

loadCurrent = evalUnPos (CurrentVar (ClassType "POINTER" [])) >>=
              flip L.load "Current"
loadOffset = evalUnPos (Var "offset" intType) >>= flip L.load "offset"