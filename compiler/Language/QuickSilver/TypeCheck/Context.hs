{-# LANGUAGE TypeSynonymInstances, MultiParamTypeClasses, FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.QuickSilver.TypeCheck.Context where

import           Control.Applicative
import           Control.Lens
import           Control.Monad.Error
import           Control.Monad.Identity
import           Control.Monad.Reader
import           Control.Monad.State

import qualified Data.HashMap.Strict as Map
import qualified Data.Text as Text
import           Data.Text (Text)

import           Language.QuickSilver.Syntax
import           Language.QuickSilver.Position
import           Language.QuickSilver.Util

import qualified Language.QuickSilver.TypeCheck.TypedExpr as T
import           Language.QuickSilver.TypeCheck.TypedExpr (TExpr)

import           Util.Monad

data TypeContext body expr = TypeContext {
      interfaces :: Map ClassName (AbsClas body expr),
      current    :: Maybe Typ,
      result     :: Typ,
      variables  :: Map Text Typ,
      pos        :: SourcePos
    }

data FlatMap body expr = 
  FlatMap { _flatPart :: Map Typ (AbsClas body expr)
          , _featPart :: Map (Typ, Text) (AbsRoutine body expr)
          }

makeLenses ''FlatMap

type TypeError = ErrorT String Identity
type FlatLookup body expr = StateT (FlatMap body expr) TypeError
type TypingBodyExpr body expr = 
  ReaderT (TypeContext body expr) (FlatLookup body expr)
type TypingBody body = TypingBodyExpr body Expr
type Typing = TypingBody (RoutineBody Expr)

instance HasClasEnv (TypeContext body expr) body expr where
    classEnv = interfaces
    update tc c = tc {interfaces = Map.insert (view className c) c (interfaces tc)}
instance ClassReader 
           (TypeContext body expr) 
           (TypingBodyExpr body expr) 
           body 
           expr

currentPos :: TypingBodyExpr body expr SourcePos
currentPos = pos <$> ask

tagPos :: a -> TypingBodyExpr body expr (Pos a)
tagPos a = currentPos >>= return . flip attachPos a

setPosition :: SourcePos -> TypingBody body a -> TypingBody body a
setPosition !p = local (\ !c -> c {pos = p})

idErrorRead :: TypingBodyExpr body expr a -> 
               TypeContext body expr -> Either String a
idErrorRead ctx = 
  runIdentity . runErrorT . fmap fst . flip runStateT (FlatMap Map.empty Map.empty) . runReaderT ctx

addFlat :: Typ -> AbsClas body expr -> TypingBodyExpr body expr ()
addFlat t flat = do
  lift $ modify (over flatPart (Map.insert t flat))
  -- lift $ modify (over featPart (Map.union flatFeatureMap))
  -- where
  --   allFeatExs = allFeatures flat
  --   flatFeatureMap = Map.fromList [((classToType flat, featureName f), f) | f <- allFeatExs]

lookupFlatFeatEx :: AbsClas body expr
                 -> Text
                 -> TypingBodyExpr body expr (Maybe (AbsRoutine body expr))
lookupFlatFeatEx cls name = do
  resMb <- lift (gets (Map.lookup (classToType cls, name) . view featPart))
  return (resMb <|> findAbsRoutine cls name)

getFlat :: Typ -> TypingBodyExpr body expr (Maybe (AbsClas body expr))
getFlat !t =
  do !flats <- lift get
     return (Map.lookup t $ view flatPart flats)

getFlat' :: Typ -> TypingBodyExpr body expr (AbsClas body expr)
getFlat' !t =
  do cfMb <- getFlat t
     cMb <- if isBasic t
            then error "getFlat': basic type"
            else (Map.lookup (classNameType t) . interfaces) <$> ask
     case cfMb <|> cMb of
       Just c -> return c
       Nothing -> error $ "getFlat': couldn't find " ++ show t

guardThrow :: Bool -> String -> TypingBody body ()
guardThrow False = throwErrorPos
guardThrow True  = const (return ())

maybeThrow :: Maybe a -> String -> TypingBody body a
maybeThrow (Just v) = const (return v)
maybeThrow Nothing  = throwErrorPos

throwErrorPos :: String -> TypingBodyExpr body expr a
throwErrorPos e = do
  p <- currentPos
  throwError (e ++ " @ " ++ show p)

currentM :: TypingBody body TExpr
currentM = do
  currentTypeMb <- current <$> ask
  case currentTypeMb of
    Just t -> tagPos (T.CurrentVar t)
    Nothing ->
      throwError "Asking for Current in module, Current only exists in classes"

mkClassCtx :: Typ -> [AbsClas body expr] -> TypeContext body expr
mkClassCtx currType cs = mkCtx (Just currType) cs

mkModuleCtx = mkCtx Nothing


mkCtx :: Maybe Typ -> [AbsClas body expr] -> TypeContext body expr
mkCtx currTypMb cs = 
    TypeContext 
    { interfaces = clasMap cs
    , current = currTypMb
    , result = error "mkCtx: no Result"
    , variables = Map.empty -- attrMap c
    , pos = error "mkCtx: no position"
    }


varCtx :: TypingBodyExpr body expr (Map Text Typ)
varCtx = fmap variables ask

typeOfVar :: Text -> TypingBodyExpr body expr (Maybe Typ)
typeOfVar "Result" = (Just . result) `fmap` ask
typeOfVar str = Map.lookup (Text.toLower str) `fmap` varCtx 

typeOfVar' :: Text -> TypingBodyExpr body expr Typ
typeOfVar' str 
    = do mV <- typeOfVar str
         m <- varCtx
         case mV of
           Just v -> return v
           Nothing -> 
               throwErrorPos (concat ["Variable not found: ", Text.unpack str, " ctx: ", show m])

addDeclsToMap :: [Decl] -> Map Text Typ -> Map Text Typ
addDeclsToMap = Map.union . declsToMap

addDecls :: [Decl] -> TypeContext body expr -> TypeContext body expr
addDecls ds ctx = ctx {variables = addDeclsToMap ds (variables ctx)}

setResult :: AbsRoutine body' expr -> 
             TypeContext body expr -> TypeContext body expr
setResult f tc = tc {result = routineResult f}
