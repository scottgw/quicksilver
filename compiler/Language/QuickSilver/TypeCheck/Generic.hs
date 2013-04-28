{-# LANGUAGE OverloadedStrings #-}
module Language.QuickSilver.TypeCheck.Generic 
       (resolveIFace, updateGeneric, updateGenerics) where

import           Control.Applicative
import           Control.Lens

import qualified Data.Text as Text
import           Data.Text (Text)

import           Language.QuickSilver.TypeCheck.Context
import qualified Language.QuickSilver.TypeCheck.TypedExpr as T

import           Language.QuickSilver.Syntax
import           Language.QuickSilver.Util
import           Language.QuickSilver.Position

import           Util.Monad

resolveIFace :: Typ -> TypingBody body (AbsClas body Expr)
resolveIFace t@(ClassType _ ts) = updateGenerics ts `fmap` lookupClass t
resolveIFace (Sep _ _ t)  = resolveIFace (ClassType t [])
resolveIFace t = error $ "resolveIFace: called on " ++ show t

type GenUpd a = Typ -> Typ -> a -> a

updateGenerics :: [Typ] -> AbsClas body expr -> AbsClas body expr
updateGenerics ts ci =
    let gs = map (\ gen -> ClassType (genericName gen) []) (view generics ci)
        f  = foldl (.) id (zipWith updateGeneric gs ts)
        newClass = f ci
    in newClass -- { generics = [] }

updateGeneric :: GenUpd (AbsClas body expr) 
updateGeneric g t = 
  classMapAttributes (updateAttribute g t) . 
  classMapRoutines (updateFeatDecl g t)

updateFeatDecl :: GenUpd (AbsRoutine body expr)
updateFeatDecl g t fd = 
    fd 
    { routineArgs = map (updateDecl g t) (routineArgs fd)
    , routineResult = updateTyp g t (routineResult fd)
    }

updateAttribute g t a = a {attrDecl = updateDecl g t (attrDecl a)}

updateDecl :: GenUpd Decl
updateDecl g t (Decl n t') = Decl n (updateTyp g t t')

updateTyp :: GenUpd Typ
updateTyp g t t'@(ClassType name types)
  | g == t' = t
  | otherwise = ClassType name (map (updateTyp g t) types)
updateTyp g t t'@(TupleType typesOrDecls)
  | g == t' = t
  | otherwise = case typesOrDecls of
    Left types -> TupleType (Left $ map (updateTyp g t) types)
    Right decls -> TupleType (Right $ map (updateDecl g t) decls)
updateTyp g t t' 
  | g == t' = t
  | otherwise =  t'
