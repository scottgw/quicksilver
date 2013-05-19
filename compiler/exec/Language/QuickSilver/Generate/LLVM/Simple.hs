module Language.QuickSilver.Generate.LLVM.Simple
    (
     IntPredicate (..), FPPredicate (..),
     CallingConvention (..),
     ContextRef, ValueRef, TypeRef,
     BasicBlockRef, BuilderRef,
     Build, Env,

     askModule, writeModuleToFile, runBuild,
     withUpdEnv, lookupEnv, fromListEnv,
     alloca, load, store,

     addGlobal, setInitializer, setGlobalConstant, getNamedGlobal,
     getFirstGlobal, getNextGlobal,

     setValueName, getValueName,
                   
     setVisibility, getVisibility,
     setLinkage, getLinkage,

     dumpValue,
     
     gep, gepInt,
     extractValue, insertValue,

     add, sub, mul, fdiv, neg, fneg,
     nott, andd, orr,
     condBr, br,
     icmp, fcmp,

     sext, trunc,
     
     ptrToInt, siToFP, bitcast,
     constPtrToInt,

     ret, retVoid,

     getInsertBlock, getEntryBasicBlock, appendBasicBlock, positionAtEnd,
     getBasicBlockParent,

     globalString, string, nul, constInt, constReal,  struct,

     unreachable,

     buildLandingPad, setCleanup, addClause,

     addFunc, funcType, funcTypeVar, getNamedFunction,
     paramTypes, countParams, getParam,
     call, call',callByName,
     invoke, invoke',
     setFunctionCallConv,
     setInstructionCallConv,
    ) where

import Control.Monad.Reader

import Data.Array.Storable
import           Data.Text (Text)
import qualified Data.Text as Text

import Foreign.C
import Foreign.Marshal.Array
import Foreign.Storable

import GHC.Ptr

import qualified LLVM.FFI.Core as L
import LLVM.FFI.Core 
    (
     BuilderRef, ValueRef, BasicBlockRef,
     CallingConvention, ContextRef,
     constPtrToInt,

     constInt, constReal
             
    )

import Language.QuickSilver.Generate.LLVM.Types
import Language.QuickSilver.Generate.LLVM.Util


setGlobalConstant :: ValueRef -> Bool -> Build ()
setGlobalConstant v b = lift (L.setGlobalConstant v b)

setVisibility :: ValueRef -> Int -> Build ()
setVisibility v i = lift $ L.setVisibility v (fromIntegral i)

getVisibility :: ValueRef -> Build Int
getVisibility v = lift $ fromIntegral `fmap` L.getVisibility v

setLinkage :: ValueRef -> Int -> Build ()
setLinkage v i = lift $ L.setLinkage v (fromIntegral i)

getLinkage :: ValueRef -> Build Int
getLinkage v = lift $ fromIntegral `fmap` L.getLinkage v

addGlobal :: TypeRef -> String -> Build ValueRef
addGlobal t s = do
    m <- askModule
    lift $ withCString s (L.addGlobal m t)

setInitializer :: ValueRef -> ValueRef -> Build ()
setInitializer = liftBuild2  L.setInitializer

setFunctionCallConv :: ValueRef -> CallingConvention -> Build ()
setFunctionCallConv v = lift . L.setFunctionCallConv v . L.fromCallingConvention


setInstructionCallConv :: ValueRef -> CallingConvention -> Build ()
setInstructionCallConv v = 
  lift . L.setInstructionCallConv v . L.fromCallingConvention

dumpValue :: ValueRef -> Build ()
dumpValue v = lift (L.dumpValue v)

ptrToInt :: ValueRef -> TypeRef -> String -> Build ValueRef
ptrToInt v t str = do
  b <- askBuild
  lift $ withCString str (L.buildPtrToInt b v t)

countParamTypes :: TypeRef -> Build Int
countParamTypes t = fromIntegral `fmap` (lift $ L.countParamTypes t)

countParams :: ValueRef -> Int
countParams = fromIntegral . L.countParams

paramTypesIO :: Int -> TypeRef -> IO [TypeRef]
paramTypesIO n t = do
  arr <- mallocArray n
  L.getParamTypes t arr
  peekArray n arr

paramTypes :: TypeRef -> Build [TypeRef]
paramTypes t = do
  n <- countParamTypes t
  lift (paramTypesIO n t)


{-
data Linkage = 

setLinkage :: ValueRef -> Linkage -> Buil ()
setLinkage v l = lift $ L.setLinkage v (fromEnum l)
-}

getParam :: ValueRef -> Int -> ValueRef
getParam funcRef = L.getParam funcRef . toEnum

getNamedFunction :: Text -> Build ValueRef
getNamedFunction s = do
  m <- askModule
  debug $ "getNamedFunction: asking for: " ++ show s
  lift $ withCString (Text.unpack s) (L.getNamedFunction m)

nul :: TypeRef -> ValueRef
nul = L.constNull

getFirstGlobal :: Build ValueRef
getFirstGlobal = askModule >>= lift . L.getFirstGlobal

getNextGlobal :: ValueRef -> Build ValueRef
getNextGlobal = lift . L.getNextGlobal 

getNamedGlobal :: Text -> Build ValueRef
getNamedGlobal str = do
  m <- askModule
  lift $ withCString (Text.unpack str) (L.getNamedGlobal m)

setValueName :: ValueRef -> String -> Build ()
setValueName v str = lift $ withCString str $ L.setValueName v

getValueName :: ValueRef -> Build String
getValueName v = lift $ L.getValueName v >>= peekCString 

globalString :: String -> Build ValueRef
globalString str = do
  b <- askBuild
  let str' = str ++ "_global"
  lift $ withCString str (withCString str' . L.buildGlobalStringPtr b)

string :: String -> Build ValueRef
string origStr = 
   let 
       str = origStr -- ++ "\0"
       strGlob = origStr ++ "_global"
       l = fromIntegral (length str)
   in do
     fmt <- lift $ withCString str ( \ cstr -> return $ L.constString cstr l False)
     t <- typeOfVal fmt
     m <- askModule
     g <- lift $  withCString strGlob (L.addGlobal m t)
     lift $ L.setInitializer g fmt
     setLinkage g 5 -- WeakAnyLinkage

     return g

struct :: Bool -> [ValueRef] -> Build ValueRef
struct packed elems =
    let cStruct ar = L.constStruct ar (fromIntegral $ length elems) packed
    in lift $ withPtrArray elems (return . cStruct)

extractValue :: ValueRef -> Int -> Build ValueRef
extractValue aggr indx = do
  b <- askBuild
  lift $ withCString "extractVal" 
           (L.buildExtractValue b aggr (fromIntegral indx))

insertValue :: ValueRef -> ValueRef -> Int -> Build ValueRef
insertValue aggr newVal indx = do
  b <- askBuild
  lift $ withCString "insertVal" 
           (L.buildInsertValue b aggr newVal (fromIntegral indx))

load :: ValueRef -> String -> Build ValueRef
load v str = askBuild >>= lift . withCString str . ($ v) . L.buildLoad

store :: ValueRef -> ValueRef -> Build ValueRef
store = withBuilder2 L.buildStore

call :: Text -> [ValueRef] -> Build ValueRef
call s args = do
  funcRef <- lookupEnv s
  call' funcRef args (Text.unpack s)

callByName :: Text -> [ValueRef] -> Build ValueRef
callByName f args = do
  fPtr <- getNamedFunction f
  call' fPtr args ("callByName: " ++ show f)

-- FIXME: return Void functions can't have a name 
-- However, we can't even examine the return type of the functino value
-- because it is reported as a pointer-type, not a function-type!
call' :: ValueRef -> [ValueRef] -> String -> Build ValueRef
call' f args _str = 
    withBuilder0 (\b -> withCString "" =<< 
                         withArrayLenC args (\p -> return . L.buildCall b f p))

withArrayLenC ::  Storable a => [a] -> (Ptr a -> CUInt -> IO b) -> IO b
withArrayLenC xs f = withArrayLen xs (\ i p -> f p (fromIntegral i))

invoke :: Text -> [ValueRef] -> BasicBlockRef -> BasicBlockRef -> 
          Build ValueRef
invoke s args b1 b2 = do
  f <- lookupEnv s
  invoke' f args b1 b2 (Text.unpack s)

invoke' :: ValueRef -> [ValueRef] -> BasicBlockRef -> BasicBlockRef -> 
          String -> Build ValueRef
invoke' f args norm excp _ = 
    withBuilder0 
    (\ b -> withCString "" ( \ s ->
            withArrayLenC args 
              (\ p i -> L.buildInvoke b f p i norm excp s))
    )

buildLandingPad :: TypeRef -> ValueRef -> Int -> String -> Build ValueRef
buildLandingPad typ personality numClauses name =
    withBuilder0 $ \b -> withCString name $ \ cstr ->
      L.buildLandingPad b typ personality (fromIntegral numClauses) cstr

addClause :: ValueRef -> ValueRef -> Build ()
addClause = liftBuild2 L.addClause

setCleanup :: ValueRef -> Bool -> Build ()
setCleanup landingpad cleanup = lift $ L.setCleanup landingpad (fromIntegral $ fromEnum $ cleanup)

alloca :: TypeRef -> Text-> Build ValueRef
alloca tr str = askBuild >>= 
                lift . withCString (Text.unpack str) . ($ tr) . L.buildAlloca

unreachable :: Build ()
unreachable = withBuilder0 L.buildUnreachable >> return ()

retVoid :: Build ()
retVoid = withBuilder0 L.buildRetVoid >> return ()

ret :: ValueRef -> Build ()
ret v = withBuilder0 (flip L.buildRet v) >> return ()

icmp :: IntPredicate -> ValueRef -> ValueRef -> String -> Build ValueRef
icmp p v1 v2 str =
    withBuilder0 ( \ b -> withCString str 
                         (L.buildICmp b (fromIntPredicate p) v1 v2)
                 )

fcmp :: FPPredicate -> ValueRef -> ValueRef -> String -> Build ValueRef
fcmp p v1 v2 str =
    withBuilder0 ( \ b -> withCString str 
                         (L.buildFCmp b (fromFPPredicate p) v1 v2)
                 )

siToFP :: ValueRef -> TypeRef -> String -> Build ValueRef
siToFP v t str = 
    withBuilder0 ( \ b -> withCString str (L.buildSIToFP b v t))

bitcast :: ValueRef -> TypeRef -> String -> Build ValueRef
bitcast v t str = 
    withBuilder0 ( \ b -> withCString str (L.buildBitCast b v t))


addFunc :: Text -> TypeRef -> Build ValueRef
addFunc str typ = do
  m <- askModule
  lift (withCString (Text.unpack str) ( \ cstr -> L.addFunction m cstr typ))

condBr :: ValueRef -> BasicBlockRef -> BasicBlockRef -> Build ()
condBr c b1 b2 = withBuilder0 (\b -> L.buildCondBr b c b1 b2) >> return ()

br :: BasicBlockRef -> Build ()
br bl = withBuilder0 (\ b -> L.buildBr b bl) >> return ()

positionAtEnd :: BasicBlockRef -> Build ()
positionAtEnd = withBuilder1 L.positionAtEnd

getInsertBlock :: Build ValueRef
getInsertBlock = withBuilder0 L.getInsertBlock

opWrap :: (BuilderRef -> ValueRef -> ValueRef -> CString -> IO ValueRef) ->
      ValueRef -> ValueRef -> String -> Build ValueRef
opWrap op v1 v2 str = 
    askBuild >>= lift . withCString str . (\ f -> f v1 v2) . op

unOpWrap :: (BuilderRef -> ValueRef -> CString -> IO ValueRef) ->
      ValueRef -> String -> Build ValueRef
unOpWrap op v str = 
    askBuild >>= lift . withCString str . ($ v) . op

add,sub,orr,mul,fdiv :: ValueRef -> ValueRef -> String -> Build ValueRef
add = opWrap L.buildAdd
sub = opWrap L.buildSub
orr = opWrap L.buildOr
andd = opWrap L.buildAnd
mul = opWrap L.buildMul
fdiv = opWrap L.buildFDiv

nott, neg :: ValueRef -> String -> Build ValueRef
nott = unOpWrap L.buildNot
neg = unOpWrap L.buildNeg
fneg = unOpWrap L.buildFNeg


sext :: ValueRef -> TypeRef -> String -> Build ValueRef
sext val extendTo str = 
  withBuilder0 $ \b -> withCString str (L.buildSExt b val extendTo)
  
trunc :: ValueRef -> TypeRef -> String -> Build ValueRef
trunc val truncTo str = 
  withBuilder0 $ \b -> withCString str (L.buildTrunc b val truncTo)
  


getEntryBasicBlock :: ValueRef -> Build BasicBlockRef
getEntryBasicBlock = liftBuild1 L.getEntryBasicBlock

appendBasicBlock :: ValueRef -> Text -> Build BasicBlockRef
appendBasicBlock v str = 
  withContext0 $ \c -> 
      withCString (Text.unpack str) (L.appendBasicBlockInContext c v)

getBasicBlockParent :: BasicBlockRef ->  Build ValueRef
getBasicBlockParent = liftBuild1 L.getBasicBlockParent

funcType :: TypeRef -> [TypeRef] -> Build TypeRef
funcType rType argTypes = functionType' rType argTypes False

funcTypeVar :: TypeRef -> [TypeRef] -> Build TypeRef
funcTypeVar rType argTypes = functionType' rType argTypes True

functionType' :: TypeRef -> [TypeRef] -> Bool -> Build TypeRef
functionType' retType argTypes isVarArg = 
    let
        ftIO typeArr = return $
            L.functionType 
             retType
             typeArr 
             (toEnum $ length argTypes) 
             (toEnum . fromEnum $ isVarArg)
    in lift $ do
      cArr <- newListArray (0, length argTypes - 1) argTypes
      withStorableArray cArr ftIO

gep :: ValueRef -> [ValueRef] -> Build ValueRef
gep v idxs = 
  let
      gepIO b ar = withCString "gep" (L.buildGEP b v ar (toEnum $ length idxs))
  in
    withBuilder0 $ \ b -> do
      iArr <- newListArray (0, length idxs - 1) idxs
      withStorableArray iArr (gepIO b)


gepInt :: ValueRef -> [Int] -> Build ValueRef
gepInt v is = do
  i32 <- int32TypeM
  gep v $ map (\ i -> constInt i32 (fromIntegral i) False) is

data IntPredicate =
    IntEQ                       -- ^ equal
  | IntNE                       -- ^ not equal
  | IntUGT                      -- ^ unsigned greater than
  | IntUGE                      -- ^ unsigned greater or equal
  | IntULT                      -- ^ unsigned less than
  | IntULE                      -- ^ unsigned less or equal
  | IntSGT                      -- ^ signed greater than
  | IntSGE                      -- ^ signed greater or equal
  | IntSLT                      -- ^ signed less than
  | IntSLE                      -- ^ signed less or equal
    deriving (Eq, Ord, Enum, Show)

fromIntPredicate :: IntPredicate -> CInt
fromIntPredicate p = fromIntegral (fromEnum p + 32)

data FPPredicate =
    FPFalse           -- ^ Always false (always folded)
  | FPOEQ             -- ^ True if ordered and equal
  | FPOGT             -- ^ True if ordered and greater than
  | FPOGE             -- ^ True if ordered and greater than or equal
  | FPOLT             -- ^ True if ordered and less than
  | FPOLE             -- ^ True if ordered and less than or equal
  | FPONE             -- ^ True if ordered and operands are unequal
  | FPORD             -- ^ True if ordered (no nans)
  | FPUNO             -- ^ True if unordered: isnan(X) | isnan(Y)
  | FPUEQ             -- ^ True if unordered or equal
  | FPUGT             -- ^ True if unordered or greater than
  | FPUGE             -- ^ True if unordered, greater than, or equal
  | FPULT             -- ^ True if unordered or less than
  | FPULE             -- ^ True if unordered, less than, or equal
  | FPUNE             -- ^ True if unordered or not equal
  | FPT               -- ^ Always true (always folded)
    deriving (Eq, Ord, Enum, Show)

fromFPPredicate :: FPPredicate -> CInt
fromFPPredicate p = fromIntegral (fromEnum p)
