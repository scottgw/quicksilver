module Language.QuickSilver.Generate.Builtin.Stdio where

import Data.Map

import Language.QuickSilver.Util

import Language.QuickSilver.Generate.Eval

import Language.QuickSilver.Generate.LLVM.Simple
import Language.QuickSilver.Generate.LLVM.Types

import Language.QuickSilver.TypeCheck.TypedExpr


featMap :: Map (String, String) (Build ())
featMap = mapKeys ((,) "STDIO") (fromList featureList)

createMap :: Map (String, String) (Build ValueRef)
createMap = mapKeys ((,) "STDIO") (fromList createList)

createList :: [(String, Build ValueRef)]
createList = []

featureList :: [(String, Build ())]
featureList = 
    [("putchar", putchar)]

putchar :: Build ()
putchar = do
  cPtr <- evalUnPos (Var "c" charType)
  c <- load cPtr "c"
  i32 <- int32TypeM
  cAsInt <- sext c i32 "extend to i32"
  r <- callByName "putchar" [cAsInt]
  ret r
