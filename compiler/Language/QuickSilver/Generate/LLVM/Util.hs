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
     currentRoutine, setRoutine,

     lookupClas, lookupValue,
     lookupClasLType,

     writeModuleToFile,
 
     withPtrArray, 

     runBuild,
     debug, debugDump
    )
    where

import           Control.Applicative
import           Control.Monad.Reader

import           Language.QuickSilver.Syntax
import           Language.QuickSilver.Util

import           Data.Array.Storable
import           Data.Hashable
import qualified Data.HashMap.Strict as Map
import           Data.Text (Text)
import qualified Data.Text as Text

import           Foreign.C
import           Foreign.Ptr
import           Foreign.Storable

import qualified LLVM.FFI.Core as L
import qualified LLVM.FFI.BitWriter as L
import           LLVM.FFI.Core 
    (
     BuilderRef,
     ValueRef,
     ModuleRef,
     ContextRef,
     TypeRef
    )

import           Prelude hiding (lookup)

type Env = Map Text ValueRef

data ClassInfo = 
    ClassInfo
    {
      rtClass       :: ClasInterface,
      rtClassStruct :: TypeRef
    } deriving Show

type ClassEnv = Map Text ClassInfo

data BuildState = 
    BuildState
    {
      bsBuilder  :: BuilderRef,
      bsModule   :: ModuleRef,
      bsContext  :: ContextRef,
      bsEnv      :: Env,
      bsRoutine  :: RoutineI, 
      bsCurrent  :: ClasInterface,
      bsClassEnv :: ClassEnv,
      bsDebug    :: Bool
    }

type Build a = ReaderT BuildState IO a

debug :: String -> Build ()
debug str = do
  debg <- bsDebug <$> ask
  when debg $ lift $ putStrLn str

debugDump :: ValueRef -> Build ()
debugDump valRef = do
  debg <- bsDebug <$> ask
  when debg $ lift $ L.dumpValue valRef

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

currentRoutine :: Build RoutineI
currentRoutine = bsRoutine `fmap` ask

writeModuleToFile :: ModuleRef -> String -> IO Bool
writeModuleToFile = (flip withCString) . L.writeBitcodeToFile

runBuild :: Bool -> Text -> Build a -> IO a
runBuild debg moduleName b = do
  context <- L.contextCreate
  builder <- L.createBuilderInContext context
  modul   <- withCString (Text.unpack moduleName)
                         (flip L.moduleCreateWithNameInContext context)

  runReaderT b (BuildState {
                  bsBuilder  = builder
                , bsModule   = modul 
                , bsContext  = context
                , bsCurrent  = error "Current class not set"
                , bsRoutine  = error "Current routine not set"
                , bsEnv      = Map.empty
                , bsClassEnv = Map.empty
                , bsDebug    = debg
                }
               )

updEnv :: (Env -> Env) -> BuildState -> BuildState
updEnv f bs = bs { bsEnv = f (bsEnv bs) }

updClassEnv :: (ClassEnv -> ClassEnv) -> BuildState -> BuildState
updClassEnv f bs = bs { bsClassEnv = f (bsClassEnv bs)}

insertClassEnv :: Text -> ClassInfo -> ClassEnv -> ClassEnv
insertClassEnv = Map.insert

fromListClassEnv :: [(Text, ClassInfo)] -> ClassEnv -> ClassEnv
fromListClassEnv pairs bs = foldr (uncurry insertClassEnv) bs pairs

insertEnv :: Text -> ValueRef -> Env -> Env
insertEnv s v = Map.insert s v

fromListEnv :: [(Text, ValueRef)] -> Env -> Env
fromListEnv pairs bs = foldr (uncurry insertEnv) bs pairs

setClassEnv :: ClassEnv -> (BuildState -> BuildState)
setClassEnv = updClassEnv . const

setCurrent :: ClasInterface -> (BuildState -> BuildState)
setCurrent c bs = bs {bsCurrent = c}

setRoutine :: RoutineI -> (BuildState -> BuildState)
setRoutine f bs = bs {bsRoutine = f}

lookupClassEnv :: Text -> Build ClassInfo
lookupClassEnv t = fmap (lookupErr (Text.unpack t) t) askClassEnv

lookupEnv :: Text -> Build ValueRef
lookupEnv t = 
    fmap (\e -> 
              lookupErr (unwords[s, "in"
                                ,unlines (map Text.unpack (Map.keys e))
                                ]) t e) askEnv
          where s = Text.unpack t


lookupEnvM :: Text -> Build (Maybe ValueRef)
lookupEnvM s = fmap (Map.lookup s) askEnv

lookupErr :: (Eq k, Hashable k) => String -> k -> Map k v -> v
lookupErr err k m = maybe (error err) id (Map.lookup k m)

lookupValue :: ClassName -> Build ValueRef
lookupValue = lookupEnv

lookupClasLType :: ClassName -> Build TypeRef
lookupClasLType = fmap rtClassStruct . lookupClassEnv

lookupClas :: ClassName -> Build ClasInterface
lookupClas  = fmap rtClass . lookupClassEnv

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
