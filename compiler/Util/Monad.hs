{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
module Util.Monad where

import           Control.Monad.Reader

import qualified Data.HashMap.Strict as Map
import qualified Data.Text as Text
import           Data.Text (Text)

import           Language.QuickSilver.Syntax
import           Language.QuickSilver.Util

class HasClasEnv r body expr | r -> body, r -> expr where
    classEnv :: r -> Map Text (AbsClas body expr)
    update   :: r -> (AbsClas body expr) -> r

class (HasClasEnv r body expr, MonadReader r m) => 
      ClassReader r m body expr | m -> r where

askClassEnv :: ClassReader r m body expr => 
               m (Map Text (AbsClas body expr))
askClassEnv = classEnv `liftM` ask

lookupClassM :: ClassReader r m body expr => 
                Typ -> m (Maybe (AbsClas body expr))
lookupClassM (ClassType cn _) = Map.lookup cn `liftM` askClassEnv
lookupClassM (Sep _ _ cn)     = lookupClassM (ClassType cn [])
lookupClassM (TupleType ls)    = lookupClassM (ClassType "TUPLE" ts)
  where getTypes (Left xs) = xs
        getTypes (Right dcls) = map declType dcls
        ts = getTypes ls
lookupClassM t = error $ "lookupClassM: can't lookup a " ++ show t ++ " type"

lookupClass :: ClassReader r m body expr => Typ -> m (AbsClas body expr)
lookupClass t = liftM (maybe (error $ "lookupClas: can't find " ++ show t) id) 
                (lookupClassM t)


-- lookupFeatureM typ name = do
--   clas <- lookupClassM typ
--   return (findFeature <$> clas <*> pure name)

lookupAttrM :: ClassReader r m body expr => Typ -> Text
               -> m (Maybe Attribute)
lookupAttrM t name = do
  c <- lookupClassM t
  return (do
           c' <- c
           findAttrInt c' name)

lookupAttr :: ClassReader r m body expr => Typ -> Text -> m Attribute
lookupAttr t name 
    = liftM (maybe 
             (error $ "lookupAttr: can't fine " ++ show t ++ "." ++ Text.unpack name)
             id)
      (lookupAttrM t name)


-- lookupRoutineM :: ClassReader r m body => Typ -> String -> m (Maybe RoutineI)
lookupRoutineM t name = do
  c <- lookupClassM t
  return (do
           c' <- c
           findRoutineInt c' name)

-- lookupRoutine :: ClassReader r m body => Typ -> String -> m RoutineI
lookupRoutine t name 
    = liftM (maybe 
             (error $ "lookupRoutine: can't fine " ++ show t ++ "." ++ Text.unpack name)
             id)
      (lookupRoutineM t name)
            
