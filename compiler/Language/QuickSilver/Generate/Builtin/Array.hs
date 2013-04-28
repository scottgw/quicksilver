module Language.QuickSilver.Generate.Builtin.Array where

import Data.Map

import Language.QuickSilver.Syntax (Typ(..))
import Language.QuickSilver.Util

import Language.QuickSilver.Generate.Eval
import Language.QuickSilver.Generate.Memory.Attribute
import Language.QuickSilver.Generate.Memory.Type
import Language.QuickSilver.Generate.Util

import Language.QuickSilver.Generate.LLVM.Simple

import Language.QuickSilver.TypeCheck.TypedExpr


arrayFeatMap :: Map (String, String) (Build ())
arrayFeatMap = mapKeys ((,) "ARRAY") (fromList featureList)

arrayCreatMap :: Map (String, String) (Build ValueRef)
arrayCreatMap = mapKeys ((,) "ARRAY") (fromList createList)

createList :: [(String, Build ValueRef)]
createList = 
    [     
     ("make",make_create)
    ]

featureList :: [(String, Build ())]
featureList = 
    [
     ("make",make)
    ,("item",item)
    ,("put" ,put)
    ]

arTyp, gTyp :: Typ
arTyp = ClassType "ARRAY" []
gTyp  = ClassType "G" []

indexPtr :: Build ValueRef
indexPtr = do
  a <- evalUnPos (CurrentVar arTyp) >>= flip load ""
  i <- evalUnPos (Var "i" intType) >>= flip load ""
  gep a [i]

item :: Build ()
item = do
  xPtr <- indexPtr
  x <- load xPtr "indexing array"
  s <- lookupEnv "Result"
  _ <- store x s
  return ()

make :: Build ()
make =  return ()

put :: Build ()
put = do
  x <- evalUnPos (Var "x" gTyp)
  xPtr <- indexPtr
  x' <- load x ""
  _ <- store x' xPtr 
  return ()

make_create :: Build ValueRef
make_create = do
  f <- getNamedFunction (featureAsCreate "ARRAY" "make")
  let n = getParam f 0
  t <- typeOfM gTyp
  arrayMalloc n t