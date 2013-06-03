module Language.QuickSilver.Generate.LLVM.Util 
    (
     Build, Env, ClassEnv, ClassInfo (..), BuildState (),
     
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

import           Foreign.Ptr
import           Foreign.Storable

import qualified LLVM.Wrapper.Core as W 
import           LLVM.Wrapper.Core ( Type, Value, BasicBlock
                                   , Builder, Context, Module
                                   , CallingConvention, FPPredicate
                                   , IntPredicate)
import qualified LLVM.Wrapper.BitWriter as W 

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
