module Language.QuickSilver.Generate.Builtin.Integer32 where

import Data.Map

import Language.QuickSilver.Util

import Language.QuickSilver.Generate.Eval

import qualified Language.QuickSilver.Generate.LLVM.Simple as L

import Language.QuickSilver.TypeCheck.TypedExpr


featMap :: Map (String, String) (L.Build ())
featMap = mapKeys ((,) "INTEGER_32") (fromList featureList)

createMap :: Map (String, String) (L.Build L.ValueRef)
createMap = mapKeys ((,) "INTEGER_32") (fromList createList)

createList :: [(String, L.Build L.ValueRef)]
createList = []

featureList :: [(String, L.Build ())]
featureList = 
    [
     ("is_less", relOpFunc L.IntSLT)
    ,("is_less_equal", relOpFunc L.IntSLE)
    ,("is_greater", relOpFunc L.IntSGT)
    ,("is_greater_equal", relOpFunc L.IntSGE)
    ,("subtract", sub)
    ,("add", add)
    ]

loadCurrent = evalUnPos (CurrentVar intType) >>= flip L.load "Current"
loadOther   = evalUnPos (Var "other" intType) >>= flip L.load "other"

relOpFunc op = do
  i1 <- loadCurrent
  i2 <- loadOther
  res <- L.icmp op i1 i2 "compare"
  L.ret res

add :: L.Build ()
add = do
  i1 <- loadCurrent
  i2 <- loadOther
  res <- L.add i1 i2 "add"
  L.ret res

sub :: L.Build ()
sub = do
  i1 <- loadCurrent
  i2 <- loadOther
  res <- L.sub i1 i2 "sub"
  L.ret res