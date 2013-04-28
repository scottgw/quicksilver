module Language.QuickSilver.Generate.LLVM.Util 
    (
     Build, Env, ClassEnv, ClassInfo (..), BuildState (),
     lift, liftBuild1, liftBuild2, liftBuild3, liftBuild4, liftBuild5,
     withBuilder0, withBuilder1, withBuilder2, withBuilder3, withBuilder4,
     withBuilder5,
     withContext0, withContext1, withContext2, withContext3, withContext4,
     withContext5,

     
     withUpdEnv,

     askBuild, askModule, askEnv, askClassEnv, askContext, singleEnv,

     updEnv, insertEnv, fromListEnv, lookupEnv, lookupEnvM,

     updClassEnv, insertClassEnv, fromListClassEnv, lookupClassEnv,
     setClassEnv,

     currentClass, setCurrent,
     currentFeature, setFeature,

     lookupClas, lookupInherit, lookupValue,
     lookupVTable, lookupClasLType,

     writeModuleToFile,
 
     withPtrArray, 

     runBuild,
     debug, debugDump
    )
    where

import Control.Applicative
import Control.Monad.Reader

import Language.QuickSilver.Syntax
import Language.QuickSilver.Util

import Data.Array.Storable
import qualified Data.Map as Map
import Data.Map (Map)

import Foreign.C
import Foreign.Ptr
import Foreign.Storable

import qualified LLVM.FFI.Core as L
import qualified LLVM.FFI.BitWriter as L
import LLVM.FFI.Core 
    (
     BuilderRef,
     ValueRef,
     ModuleRef,
     ContextRef,
     TypeRef
    )

import Prelude hiding (lookup)

type Env = Map String ValueRef

data ClassInfo = 
    ClassInfo
    {
      rtClass       :: ClasInterface,
      rtClassStruct :: TypeRef,
      rtVTable      :: ValueRef
    } deriving Show

type ClassEnv = Map String ClassInfo

data BuildState = 
    BuildState
    {
      bsBuilder  :: BuilderRef,
      bsModule   :: ModuleRef,
      bsContext  :: ContextRef,
      bsEnv      :: Env,
      bsFeature  :: RoutineI, 
      bsCurrent  :: ClasInterface,
      bsClassEnv :: ClassEnv,
      bsDebug    :: Bool
    }

type Build a = ReaderT BuildState IO a

debug :: String -> Build ()
debug str = do
  debug <- bsDebug <$> ask
  when debug $ lift $ putStrLn str

debugDump :: ValueRef -> Build ()
debugDump valRef = do
  debug <- bsDebug <$> ask
  when debug $ lift $ L.dumpValue valRef

withPtrArray :: Storable a => [a] -> (Ptr a -> IO b) -> IO b
withPtrArray as f = do
  arr <- newListArray (0, length as - 1) as
  withStorableArray arr f

singleEnv :: Decl -> ValueRef -> Env
singleEnv (Decl n _) v = Map.singleton n v

withUpdEnv :: (Env -> Env) -> Build a -> Build a
withUpdEnv = local . updEnv

askBuild :: Build BuilderRef
askBuild = fmap bsBuilder ask

askModule :: Build ModuleRef 
askModule = fmap bsModule ask

askContext :: Build ContextRef
askContext = fmap bsContext ask

askEnv :: Build Env
askEnv = fmap bsEnv ask

askClassEnv :: Build ClassEnv
askClassEnv = fmap bsClassEnv ask

currentClass :: Build ClasInterface
currentClass = fmap bsCurrent ask

currentFeature :: Build RoutineI
currentFeature = bsFeature `fmap` ask

writeModuleToFile :: ModuleRef -> String -> IO CInt
writeModuleToFile = (flip withCString) . L.writeBitcodeToFile

runBuild :: Bool -> String -> Build a -> IO a
runBuild debug moduleName b = do
  context <- L.contextCreate
  builder <- L.createBuilderInContext context
  modul   <- withCString moduleName 
                         (flip L.moduleCreateWithNameInContext context)

  runReaderT b (BuildState {
                  bsBuilder  = builder
                , bsModule   = modul 
                , bsContext  = context
                , bsCurrent  = error "Current class not set"
                , bsFeature  = error "Current feature not set"
                , bsEnv      = Map.empty
                , bsClassEnv = Map.empty
                , bsDebug    = debug
                }
               )

updEnv :: (Env -> Env) -> BuildState -> BuildState
updEnv f bs = bs { bsEnv = f (bsEnv bs) }

updClassEnv :: (ClassEnv -> ClassEnv) -> BuildState -> BuildState
updClassEnv f bs = bs { bsClassEnv = f (bsClassEnv bs)}

insertClassEnv :: String -> ClassInfo -> ClassEnv -> ClassEnv
insertClassEnv = Map.insert

fromListClassEnv :: [(String, ClassInfo)] -> ClassEnv -> ClassEnv
fromListClassEnv pairs bs = foldr (uncurry insertClassEnv) bs pairs

insertEnv :: String -> ValueRef -> Env -> Env
insertEnv s v = Map.insert s v

fromListEnv :: [(String, ValueRef)] -> Env -> Env
fromListEnv pairs bs = foldr (uncurry insertEnv) bs pairs

setClassEnv :: ClassEnv -> (BuildState -> BuildState)
setClassEnv = updClassEnv . const

setCurrent :: ClasInterface -> (BuildState -> BuildState)
setCurrent c bs = bs {bsCurrent = c}

setFeature :: RoutineI -> (BuildState -> BuildState)
setFeature f bs = bs {bsFeature = f}

lookupClassEnv :: String -> Build ClassInfo
lookupClassEnv s = fmap (lookupErr s s) askClassEnv

lookupEnv :: String -> Build ValueRef
lookupEnv s = fmap (\e -> lookupErr (s ++ " in " ++ show (Map.keys e)) s e) askEnv


lookupEnvM :: String -> Build (Maybe ValueRef)
lookupEnvM s = fmap (Map.lookup s) askEnv

lookupErr :: Ord k => String -> k -> Map k v -> v
lookupErr err k m = maybe (error err) id (Map.lookup k m)

lookupValue :: ClassName -> Build ValueRef
lookupValue = lookupEnv

lookupClasLType :: ClassName -> Build TypeRef
lookupClasLType = fmap rtClassStruct . lookupClassEnv

lookupClas :: ClassName -> Build ClasInterface
lookupClas  = fmap rtClass . lookupClassEnv

lookupVTable :: ClassName -> Build ValueRef
lookupVTable = fmap rtVTable . lookupClassEnv

lookupInherit :: AbsClas body exp -> Build [ClasInterface]
lookupInherit = mapM (lookupClas . classNameType . inheritClass) . allInherited

{-
building lifters, just in case
-}

liftBuild1 :: (a -> IO x) -> a -> Build x
liftBuild1 f a = lift $ f a

liftBuild2 :: (a -> b -> IO x) -> 
              a -> b -> Build x
liftBuild2 f a b = lift $ f a b

liftBuild3 :: (a -> b -> c -> IO x) -> 
              a -> b -> c -> Build x
liftBuild3 f a b c = lift $ f a b c

liftBuild4 :: (a -> b -> c -> d -> IO x) -> 
              a -> b -> c -> d -> Build x
liftBuild4 f a b c d = lift $ f a b c d

liftBuild5 :: (a -> b -> c -> d -> e -> IO x) -> 
              a -> b -> c -> d -> e -> Build x
liftBuild5 f a b c d e = lift $ f a b c d e

withBuilder0 :: (BuilderRef -> IO x) -> Build x
withBuilder0 f = askBuild >>= lift . f

withBuilder1 :: (BuilderRef -> a -> IO x) -> a -> Build x
withBuilder1 f a = askBuild >>= \ bRef -> lift $ f bRef a

withBuilder2 :: (BuilderRef -> a -> b -> IO x) -> 
                a -> b -> Build x
withBuilder2 f a b = askBuild >>= \ bRef -> lift $ f bRef a b

withBuilder3 :: (BuilderRef -> a -> b -> c -> IO x) -> 
                a -> b -> c -> Build x
withBuilder3 f a b c = askBuild >>= \ bRef -> lift $ f bRef a b c

withBuilder4 :: (BuilderRef -> a -> b -> c -> d -> IO x) -> 
                a -> b -> c -> d -> Build x
withBuilder4 f a b c d = askBuild >>= \ bRef -> lift $ f bRef a b c d

withBuilder5 :: (BuilderRef -> a -> b -> c -> d -> e -> IO x) -> 
                a -> b -> c -> d -> e -> Build x
withBuilder5 f a b c d e= askBuild >>= \ bRef -> lift $ f bRef a b c d e 

withContext0 :: (ContextRef -> IO x) -> Build x
withContext0 f = askContext >>= lift . f

withContext1 :: (ContextRef -> a -> IO x) -> a -> Build x
withContext1 f a = askContext >>= \ bRef -> lift $ f bRef a

withContext2 :: (ContextRef -> a -> b -> IO x) -> 
                a -> b -> Build x
withContext2 f a b = askContext >>= \ bRef -> lift $ f bRef a b

withContext3 :: (ContextRef -> a -> b -> c -> IO x) -> 
                a -> b -> c -> Build x
withContext3 f a b c = askContext >>= \ bRef -> lift $ f bRef a b c

withContext4 :: (ContextRef -> a -> b -> c -> d -> IO x) -> 
                a -> b -> c -> d -> Build x
withContext4 f a b c d = askContext >>= \ bRef -> lift $ f bRef a b c d

withContext5 :: (ContextRef -> a -> b -> c -> d -> e -> IO x) -> 
                a -> b -> c -> d -> e -> Build x
withContext5 f a b c d e= askContext >>= \ bRef -> lift $ f bRef a b c d e 
