{-# LANGUAGE Rank2Types #-}
module Language.QuickSilver.Generate.LLVM.Build where

import           Control.Applicative
import           Control.Monad.Reader

import           Language.QuickSilver.Syntax
import           Language.QuickSilver.TypeCheck.TypedExpr

import           Data.Array.Storable
import           Data.Char
import           Data.Hashable
import qualified Data.HashMap.Strict as Map
import           Data.List (lookup)
import           Data.Text (Text)
import qualified Data.Text as Text

import           Foreign.Ptr
import           Foreign.Storable

import qualified LLVM.Wrapper.Core as W

import           Prelude hiding (lookup)

import           Language.QuickSilver.Generate.LLVM.Simple

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
    { bsEnv      :: Env
    , bsRoutine  :: RoutineI
    , bsQueues   :: [(TExpr, Value)]
    , bsCurrent  :: ClasInterface
    , bsClassEnv :: ClassEnv
    , bsTypeEnv  :: Map Text Type -- For special types like processor.
    , bsDebug    :: Bool
    }

type Build a = ReaderT BuildState LLVMReader a

runBuild :: forall a . Bool -> Text -> Build a -> IO a
runBuild debg moduleName m = do
  context <- W.contextCreate
  builder <- W.createBuilderInContext context
  modul   <- W.moduleCreateWithNameInContext (Text.unpack moduleName) context

  let llvmData = LLVMData builder context modul
      buildState =
        BuildState { bsCurrent  = error "Current class not set"
                   , bsRoutine  = error "Current routine not set"
                   , bsQueues   = []
                   , bsEnv      = Map.empty
                   , bsClassEnv = Map.empty
                   , bsTypeEnv  = Map.empty
                   , bsDebug    = debg
                   }

      buildResult = runReaderT m buildState
  
  runLLVMReader buildResult llvmData

debug :: String -> Build ()
debug str = do
  debg <- bsDebug <$> ask
  when debg $ liftIO $ putStrLn str

debugDump :: Value -> Build ()
debugDump val = do
  debg <- bsDebug <$> ask
  when debg $ liftIO $ W.dumpValueToString val >>= putStrLn

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

askEnv :: Build Env
askEnv = bsEnv <$> ask

askClassEnv :: Build ClassEnv
askClassEnv = bsClassEnv <$> ask

currentClass :: Build ClasInterface
currentClass = bsCurrent <$> ask

currentRoutine :: Build RoutineI
currentRoutine = bsRoutine `fmap` ask

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


call' :: Text -> [Value] -> Build Value
call' s args = do
  funcRef <- lookupEnv s
  call funcRef args


invoke' :: Text -> [Value] -> BasicBlock -> BasicBlock -> Build Value
invoke' s args b1 b2 = do
  f <- lookupEnv s
  invoke f args b1 b2 (Text.unpack s)


funcType :: Type -> [Type] -> Type
funcType rType argTypes = functionType rType argTypes False

funcTypeVar :: Type -> [Type] -> Type
funcTypeVar rType argTypes = functionType rType argTypes True

int1TypeM :: LLVM m => m Type
int1TypeM = intType 1

int8TypeM :: LLVM m => m Type
int8TypeM = intType 8

int16TypeM :: LLVM m => m Type
int16TypeM = intType 16

int32TypeM :: LLVM m => m Type
int32TypeM = intType 32

int64TypeM :: LLVM m => m Type
int64TypeM = intType 64

pointer0 :: Type -> Type
pointer0 = (`W.pointerType` 0)

ptr :: Build Type
ptr = pointer0 <$> int8TypeM

gepInt :: LLVM m => Value -> [Int] -> m Value
gepInt v is = do
  i32 <- int32TypeM
  gep v $ map (\ i -> W.constInt i32 (fromIntegral i) True) is

true, false :: Build Value
true  = constInt <$> int1TypeM <*> pure 1 <*> pure False
false = constInt <$> int1TypeM <*> pure 0 <*> pure False

char :: Char -> Build Value
char c = constInt <$> int8TypeM <*> pure (fromIntegral $ ord c) <*> pure False

int :: Int -> Build Value
int i = constInt <$> int64TypeM <*> pure (fromIntegral i) <*> pure False

dbl :: Double -> Build Value
dbl d = 
    let toRealFloat = uncurry encodeFloat (decodeFloat d)
    in constReal <$> doubleType <*> pure toRealFloat
