{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.QuickSilver.TypeCheck.BasicTypes 
       (guardTypeIs, numericCanBe) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Error

import           Data.List (find)
import qualified Data.Text as Text
import           Data.Text (Text)

import           Language.QuickSilver.Syntax as S
import           Language.QuickSilver.Position
import           Language.QuickSilver.Util

import qualified Language.QuickSilver.TypeCheck.TypedExpr as T
import           Language.QuickSilver.TypeCheck.TypedExpr (TExpr)
import           Language.QuickSilver.TypeCheck.Context
import           Language.QuickSilver.TypeCheck.Generic

import           Util.Monad

numericCanBe (T.LitInt 0) t =
  isIntegerType t || isNaturalType t || isRealType t
numericCanBe (T.LitDouble n) t = isRealType t
numericCanBe (T.LitInt i) t
  | isIntegerType t || isNaturalType t =
    let (lower, upper) = typeBounds t
    in lower <= fromIntegral i && fromIntegral i <= upper
  | otherwise = False
numericCanBe e t 
  | T.texprTyp e == (ClassType "INTEGER_32" []) &&
    (t == (ClassType "REAL_64" []) || t == (ClassType "REAL_32" [])) = True
  | T.texprTyp e == (ClassType "INTEGER_32" []) &&
    t == (ClassType "INTEGER_64" []) = True
  | isNaturalType (T.texprTyp e) && --  == ClassType "NATURAL_8" [] &&
    (t == ClassType "INTEGER_32" [] || t == ClassType "NATURAL_32" []) = True
  | isIntegerType (T.texprTyp e) &&
    (t == ClassType "INTEGER_32" [] || t == ClassType "NATURAL_32" []) = True
  | otherwise = False

guardTypePred :: (Typ -> Bool) -> String -> Typ -> TypingBody body Typ
guardTypePred p s t = guardThrow (p t) s >> return t

guardTypeIs typ expr = 
  let exprType = T.texpr expr
  in guardTypePred (== typ) 
                   ("require " ++ show typ ++ " actual " ++ show exprType)
                   (T.texpr expr)
