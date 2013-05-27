{-# LANGUAGE OverloadedStrings #-}
module Language.QuickSilver.TypeCheck.Generic 
       (resolveIFace, replaceType, updateGenerics) where

import           Control.Lens

import           Data.Generics

import           Language.QuickSilver.TypeCheck.Context
import           Language.QuickSilver.Syntax
import           Language.QuickSilver.Util

import           Util.Monad

resolveIFace :: (Data body, Typeable body)
             => Typ -> TypingBody body (AbsClas body Expr)
resolveIFace t@(ClassType _ ts) = updateGenerics ts `fmap` lookupClass t
resolveIFace (Sep _ _ t)  = resolveIFace t
resolveIFace t = error $ "resolveIFace: called on " ++ show t

updateGenerics :: (Data body, Typeable body)
               => [Typ] -> AbsClas body Expr -> AbsClas body Expr
updateGenerics ts ci =
    let gs = view generics ci
        f  = foldl (.) id (zipWith replaceType gs ts)
        newClass = f ci
    in newClass -- { generics = [] }

