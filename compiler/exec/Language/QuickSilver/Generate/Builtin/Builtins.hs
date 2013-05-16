module Language.QuickSilver.Generate.Builtin.Builtins where

import qualified Data.Map as Map

import Language.QuickSilver.Syntax
import Language.QuickSilver.TypeCheck.TypedExpr

import Language.QuickSilver.Generate.LLVM.Simple
import Language.QuickSilver.Generate.LLVM.Util

import qualified Language.QuickSilver.Generate.Builtin.Array as Array
import qualified Language.QuickSilver.Generate.Builtin.Stdio as Stdio
import Language.QuickSilver.Generate.Builtin.BasicArray
import qualified Language.QuickSilver.Generate.Builtin.Integer32 as Integer32
import qualified Language.QuickSilver.Generate.Builtin.Pointer as Pointer

real64ArrayDesc = mkBasicArray (ClassType "REAL_64" [])
charArrayDesc = mkBasicArray (ClassType "CHARACTER_8" [])

featMap = Map.unions $ 
          [ Array.arrayFeatMap
          , arrayFunctions real64ArrayDesc
          , Integer32.featMap
          , Stdio.featMap
          , Pointer.featMap
          ] ++ 
          map arrayFunctions [real64ArrayDesc, charArrayDesc]
createMap = Map.unions $
            [Array.arrayCreatMap
            , Pointer.creatMap
            , Stdio.createMap
            ] ++ 
            map createArray [real64ArrayDesc, charArrayDesc]

genBuiltin :: TRoutine -> Build ()
genBuiltin rout = do
  cls <- currentClass
  case Map.lookup (className cls, routineName rout) featMap of
    Just b -> b
    Nothing -> 
      error $ "genBuiltin: couldn't find " ++ 
      show (className cls, routineName rout)

genBuiltinCreate :: String -> String -> Build ValueRef
genBuiltinCreate cname fname = 
  case Map.lookup (cname, fname) createMap of
    Just b -> b
    Nothing ->
      error $ "genBuiltinCreate: couldn't find " ++ show (cname, fname)

needsBuiltinCreate cname fname =
  Map.member (cname, fname) createMap
