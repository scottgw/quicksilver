{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Util.Monad where

import Control.Monad.Reader

import Data.Map

import Language.Eiffel.Syntax
import Language.Eiffel.Feature
import Language.Eiffel.Syntax
import Language.Eiffel.Decl

class HasClasEnv r where
    classEnv :: r -> Map String ClasInterface
    update   :: r -> ClasInterface -> r

class (HasClasEnv r, MonadReader r m) => ClassReader r m | m -> r where

askClassEnv :: ClassReader r m => m (Map String ClasInterface)
askClassEnv = classEnv `liftM` ask

lookupClassM :: ClassReader r m => Typ -> m (Maybe ClasInterface)
lookupClassM (ClassType cn _) = Data.Map.lookup cn `liftM` askClassEnv
lookupClassM (Sep _ _ cn)     = lookupClassM (ClassType cn [])
lookupClassM t = error $ "lookupClassM: can't lookup a " ++ show t ++ " type"

lookupClass :: ClassReader r m => Typ -> m ClasInterface
lookupClass t = liftM (maybe (error $ "lookupClas: can't find " ++ show t) id) 
                (lookupClassM t)

lookupAttrM :: ClassReader r m => Typ -> String -> m (Maybe Decl)
lookupAttrM t name = do
  c <- lookupClassM t
  return (do
           c' <- c
           findAttrInt c' name)

lookupAttr :: ClassReader r m => Typ -> String -> m Decl
lookupAttr t name 
    = liftM (maybe 
             (error $ "lookupAttr: can't fine " ++ show t ++ "." ++ name)
             id)
      (lookupAttrM t name)


lookupFeatureM :: ClassReader r m => Typ -> String -> m (Maybe FeatureI)
lookupFeatureM t name = do
  c <- lookupClassM t
  return (do
           c' <- c
           findFeatureInt c' name)

lookupFeature :: ClassReader r m => Typ -> String -> m FeatureI
lookupFeature t name 
    = liftM (maybe 
             (error $ "lookupFeature: can't fine " ++ show t ++ "." ++ name)
             id)
      (lookupFeatureM t name)
            