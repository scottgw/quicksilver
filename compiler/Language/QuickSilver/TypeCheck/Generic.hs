{-# LANGUAGE OverloadedStrings #-}
module Language.QuickSilver.TypeCheck.Generic 
       (resolveIFace, updateGeneric, updateGenerics) where

import           Control.Lens

import           Data.Generics

import           Language.QuickSilver.TypeCheck.Context
import           Language.QuickSilver.Syntax

import           Util.Monad

resolveIFace :: (Data body, Typeable body)
             => Typ -> TypingBody body (AbsClas body Expr)
resolveIFace t@(ClassType _ ts) = updateGenerics ts `fmap` lookupClass t
resolveIFace (Sep _ _ t)  = resolveIFace t
resolveIFace t = error $ "resolveIFace: called on " ++ show t

type GenUpd a = Typ -> Typ -> a -> a

updateGenerics :: (Data body, Typeable body)
               => [Typ] -> AbsClas body Expr -> AbsClas body Expr
updateGenerics ts ci =
    let gs = map (\ gen -> ClassType (genericName gen) []) (view generics ci)
        f  = foldl (.) id (zipWith updateGeneric gs ts)
        newClass = f ci
    in newClass -- { generics = [] }

updateGeneric :: (Data body, Typeable body) => GenUpd (AbsClas body Expr)
updateGeneric g t = everywhere (mkT updateType)
    where
      updateType t'@(ClassType name types)
          | g == t' = t
          | otherwise = ClassType name (map updateType types)
      updateType t' 
          | g == t' = t
          | otherwise =  t'

