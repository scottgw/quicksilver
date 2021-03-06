{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.QuickSilver.TypeCheck.BasicTypes 
       (guardTypeIs, numericCanBe) where

import           Language.QuickSilver.Syntax as S
import           Language.QuickSilver.Util
import qualified Language.QuickSilver.TypeCheck.TypedExpr as T
import           Language.QuickSilver.TypeCheck.Context
import           Language.QuickSilver.TypeCheck.TypeError

numericCanBe (T.LitInt 0 _t) t =
  isIntegerType t || isNaturalType t || isRealType t
numericCanBe (T.LitDouble _n) t = isRealType t
numericCanBe (T.LitInt i _t) t
  | isIntegerType t || isNaturalType t =
    let (lower, upper) = typeBounds t
    in lower <= fromIntegral i && fromIntegral i <= upper
  | otherwise = False
numericCanBe _ _ = False
-- numericCanBe e t 
--   | T.texprTyp e == IntType && t == DoubleType
--     (t == (ClassType "REAL_64" []) || t == (ClassType "REAL_32" [])) = True
--   | T.texprTyp e == (ClassType "INTEGER_32" []) &&
--     t == (ClassType "INTEGER_64" []) = True
--   | isNaturalType (T.texprTyp e) && --  == ClassType "NATURAL_8" [] &&
--     (t == ClassType "INTEGER_32" [] || t == ClassType "NATURAL_32" []) = True
--   | isIntegerType (T.texprTyp e) &&
--     (t == ClassType "INTEGER_32" [] || t == ClassType "NATURAL_32" []) = True
--   | otherwise = False

guardTypePred :: (Typ -> Bool) -> TypeError -> Typ -> TypingBody body Typ
guardTypePred p e t = guardThrow (p t) e >> return t

guardTypeIs typ expr = 
  let exprType = T.texpr expr
  in guardTypePred (== typ) 
                   (TypeMismatch typ exprType)
                   (T.texpr expr)

