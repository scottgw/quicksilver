module Language.QuickSilver.Generate.LLVM.Util 
    (
     Build, Env, ClassEnv, ClassInfo (..), BuildState (),
     lift, liftBuild1, liftBuild2, liftBuild3, liftBuild4, liftBuild5,
     withBuilder0, withBuilder1, withBuilder2, withBuilder3, withBuilder4,
     withBuilder5,
     withContext0, withContext1, withContext2, withContext3, withContext4,
     withContext5,

     
     withUpdEnv,

     askBuild, askModule, askEnv, askClassEnv, askContext,

     singleEnv, singleEnv',

     updEnv, insertEnv, fromListEnv, lookupEnv, lookupEnvM,

     updClassEnv, insertClassEnv, fromListClassEnv, lookupClassEnv,
     setClassEnv,

     insertNamedType, lookupNamedType,

     currentClass, setCurrent,
     currentRoutine, setRoutine,

     lookupClas, lookupValue, lookupQueueFor, updateQueues,
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
import           Language.QuickSilver.TypeCheck.TypedExpr

import           Data.Array.Storable
import           Data.Hashable
import qualified Data.HashMap.Strict as Map
import           Data.List (lookup)
import           Data.Text (Text)
import qualified Data.Text as Text

import           Foreign.C
import           Foreign.Ptr
import           Foreign.Storable

import qualified LLVM.Wrapper.Core as W 
import           LLVM.Wrapper.Core ( Type, Value, BasicBlock
                                   , Builder, Context, Module
                                   , CallingConvention, FPPredicate
                                   , IntPredicate)
import qualified LLVM.Wrapper.BitWriter as W 
-- import qualified LLVM.FFI.Core as L
-- import qualified LLVM.FFI.BitWriter as L
-- import           LLVM.FFI.Core 
--     (
--     )

import           Prelude hiding (lookup)

type Env = Map Text Value

data ClassInfo = 
    ClassInfo
    {
      rtClass       :: ClasInterface,
      rtClassStruct :: Either Type Type
    } deriving Show

type ClassEnv = Map Text ClassInfo

data BuildState = 
    BuildState
    {
      bsBuilder  :: Builder,
      bsModule   :: Module,
      bsContext  :: Context,
      bsEnv      :: Env,
      bsRoutine  :: RoutineI,
      bsQueues   :: [(TExpr, Value)],
      bsCurrent  :: ClasInterface,
      bsClassEnv :: ClassEnv,
      bsTypeEnv  :: Map Text Type, -- For special types like processor.
      bsDebug    :: Bool
    }

type Build a = ReaderT BuildState IO a

debug :: String -> Build ()
debug str = do
  debg <- bsDebug <$> ask
  when debg $ lift $ putStrLn str

debugDump :: Value -> Build ()
debugDump val = do
  debg <- bsDebug <$> ask
  when debg $ lift $ W.dumpValueToString val >>= putStrLn

withPtrArray :: Storable a => [a] -> (Ptr a -> IO b) -> IO b
withPtrArray as f = do
  arr <- newListArray (0, length as - 1) as
  withStorableArray arr f

singleEnv :: Decl -> Value -> Env
singleEnv (Decl n _) v = singleEnv' n v

singleEnv' :: Text -> Value -> Env
singleEnv' n v = Map.singleton n v

withUpdEnv :: (Env -> Env) -> Build a -> Build a
withUpdEnv = local . updEnv

askBuild :: Build Builder
askBuild = fmap bsBuilder ask

askModule :: Build Module 
askModule = fmap bsModule ask

askContext :: Build Context
askContext = fmap bsContext ask

askEnv :: Build Env
askEnv = fmap bsEnv ask

askClassEnv :: Build ClassEnv
askClassEnv = fmap bsClassEnv ask

currentClass :: Build ClasInterface
currentClass = fmap bsCurrent ask

currentRoutine :: Build RoutineI
currentRoutine = bsRoutine `fmap` ask

writeModuleToFile :: Module -> String -> IO ()
writeModuleToFile = W.writeBitcodeToFile

runBuild :: Bool -> Text -> Build a -> IO a
runBuild debg moduleName b = do
  context <- W.contextCreate
  builder <- W.createBuilderInContext context
  modul   <- W.moduleCreateWithNameInContext (Text.unpack moduleName) context

  runReaderT b (BuildState {
                  bsBuilder  = builder
                , bsModule   = modul
                , bsContext  = context
                , bsCurrent  = error "Current class not set"
                , bsRoutine  = error "Current routine not set"
                , bsQueues   = []
                , bsEnv      = Map.empty
                , bsClassEnv = Map.empty
                , bsTypeEnv  = Map.empty
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

insertEnv :: Text -> Value -> Env -> Env
insertEnv s v = Map.insert s v

fromListEnv :: [(Text, Value)] -> Env -> Env
fromListEnv pairs bs = foldr (uncurry insertEnv) bs pairs

insertNamedType :: Text -> Type -> BuildState -> BuildState
insertNamedType name typ bs =
    bs { bsTypeEnv = Map.insert name typ (bsTypeEnv bs) }

lookupNamedType :: Text -> Build (Maybe Type)
lookupNamedType name = Map.lookup name <$> bsTypeEnv <$> ask

lookupQueueFor :: TExpr -> Build Value
lookupQueueFor e =
    do m <- bsQueues <$> ask
       case lookup e m of
         Just v -> return v
         Nothing -> error $ "lookupQueueFor: not found " ++ show e ++
                            " in " ++ show m
      
updateQueues :: [(TExpr, Value)] -> BuildState -> BuildState
updateQueues qs bs =
    bs { bsQueues = upd (bsQueues bs) }
    where
      upd = (qs ++) -- Map.union (Map.fromList qs)

setClassEnv :: ClassEnv -> (BuildState -> BuildState)
setClassEnv = updClassEnv . const

setCurrent :: ClasInterface -> (BuildState -> BuildState)
setCurrent c bs = bs {bsCurrent = c}

setRoutine :: RoutineI -> (BuildState -> BuildState)
setRoutine f bs = bs {bsRoutine = f}

lookupClassEnv :: Text -> Build ClassInfo
lookupClassEnv t = fmap (lookupErr (Text.unpack t) t) askClassEnv

lookupEnv :: Text -> Build Value
lookupEnv t = 
    fmap (\e -> 
              lookupErr (unwords[s, "in"
                                ,unlines (map Text.unpack (Map.keys e))
                                ]) t e) askEnv
          where s = Text.unpack t


lookupEnvM :: Text -> Build (Maybe Value)
lookupEnvM s = fmap (Map.lookup s) askEnv

lookupErr :: (Eq k, Hashable k) => String -> k -> Map k v -> v
lookupErr err k m = maybe (error err) id (Map.lookup k m)

lookupValue :: ClassName -> Build Value
lookupValue = lookupEnv

lookupClasLType :: ClassName -> Build (Either Type Type)
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

withBuilder0 :: (Builder -> IO x) -> Build x
withBuilder0 f = askBuild >>= lift . f

withBuilder1 :: (Builder -> a -> IO x) -> a -> Build x
withBuilder1 f a = askBuild >>= \ bRef -> lift $ f bRef a

withBuilder2 :: (Builder -> a -> b -> IO x) -> 
                a -> b -> Build x
withBuilder2 f a b = askBuild >>= \ bRef -> lift $ f bRef a b

withBuilder3 :: (Builder -> a -> b -> c -> IO x) -> 
                a -> b -> c -> Build x
withBuilder3 f a b c = askBuild >>= \ bRef -> lift $ f bRef a b c

withBuilder4 :: (Builder -> a -> b -> c -> d -> IO x) -> 
                a -> b -> c -> d -> Build x
withBuilder4 f a b c d = askBuild >>= \ bRef -> lift $ f bRef a b c d

withBuilder5 :: (Builder -> a -> b -> c -> d -> e -> IO x) -> 
                a -> b -> c -> d -> e -> Build x
withBuilder5 f a b c d e= askBuild >>= \ bRef -> lift $ f bRef a b c d e 

withContext0 :: (Context -> IO x) -> Build x
withContext0 f = askContext >>= lift . f

withContext1 :: (Context -> a -> IO x) -> a -> Build x
withContext1 f a = askContext >>= \ bRef -> lift $ f bRef a

withContext2 :: (Context -> a -> b -> IO x) -> 
                a -> b -> Build x
withContext2 f a b = askContext >>= \ bRef -> lift $ f bRef a b

withContext3 :: (Context -> a -> b -> c -> IO x) -> 
                a -> b -> c -> Build x
withContext3 f a b c = askContext >>= \ bRef -> lift $ f bRef a b c

withContext4 :: (Context -> a -> b -> c -> d -> IO x) -> 
                a -> b -> c -> d -> Build x
withContext4 f a b c d = askContext >>= \ bRef -> lift $ f bRef a b c d

withContext5 :: (Context -> a -> b -> c -> d -> e -> IO x) -> 
                a -> b -> c -> d -> e -> Build x
withContext5 f a b c d e= askContext >>= \ bRef -> lift $ f bRef a b c d e 
