module Language.QuickSilver.Generate.LLVM.Simple
    (
     IntPredicate (..), FPPredicate (..),
     CallingConvention (..),
     ContextRef, Value, TypeRef,
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

     add, sub, mul, fdiv, neg, fneg,
     sdiv, srem,
     nott, andd, orr,
     condBr, br,
     icmp, fcmp,

     sext, trunc,
     
     ptrToInt, intToPtr, siToFP, bitcast,
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
     setInstructionCallConv
    ) where

import Control.Applicative
import Control.Monad.Reader

import Data.Array.Storable
import           Data.Text (Text)
import qualified Data.Text as Text

import Foreign.C
import Foreign.Marshal.Array
import Foreign.Storable

import GHC.Ptr

import qualified LLVM.Wrapper.Core as W 
import           LLVM.Wrapper.Core ( Type, Value, BasicBlock, Builder
                                   , CallingConvention, FPPredicate
                                   , IntPredicate)

import qualified LLVM.FFI.Core as L
-- import LLVM.FFI.Core 
--     (
--      constPtrToInt,
--      constInt, constReal
             
--     )

import Language.QuickSilver.Generate.LLVM.Types
import Language.QuickSilver.Generate.LLVM.Util


setGlobalConstant :: Value -> Bool -> Build ()
setGlobalConstant v b = lift (L.setGlobalConstant v b)

setVisibility :: Value -> Int -> Build ()
setVisibility v i = lift $ L.setVisibility v (fromIntegral i)

getVisibility :: Value -> Build Int
getVisibility v = lift $ fromIntegral `fmap` L.getVisibility v

setLinkage :: Value -> Int -> Build ()
setLinkage v i = lift $ L.setLinkage v (fromIntegral i)

getLinkage :: Value -> Build Int
getLinkage v = lift $ fromIntegral `fmap` L.getLinkage v

addGlobal :: Type -> String -> Build Value
addGlobal t s = do
    m <- askModule
    lift $ W.addGlobal m t s

setInitializer :: Value -> Value -> Build ()
setInitializer = liftBuild2  L.setInitializer

setFunctionCallConv :: Value -> CallingConvention -> Build ()
setFunctionCallConv v = 
    lift . L.setFunctionCallConv v . L.fromCallingConvention

setInstructionCallConv :: Value -> CallingConvention -> Build ()
setInstructionCallConv v = 
  lift . L.setInstructionCallConv v . L.fromCallingConvention

dumpValue :: Value -> Build ()
dumpValue v = lift (L.dumpValue v)

ptrToInt :: Value -> Type -> String -> Build Value
ptrToInt = withBuilder3 W.buildPtrToInt

intToPtr :: Value -> Type -> String -> Build Value
intToPtr = withBuilder3 W.buildIntToPtr

countParamTypes :: Type -> Build Int
countParamTypes t = fromIntegral `fmap` (lift $ L.countParamTypes t)

countParams :: Value -> Int
countParams = fromIntegral . L.countParams

paramTypesIO :: Int -> Type -> IO [Type]
paramTypesIO n t = do
  arr <- mallocArray n
  L.getParamTypes t arr
  peekArray n arr

paramTypes :: Type -> Build [Type]
paramTypes t = do
  n <- countParamTypes t
  lift (paramTypesIO n t)


{-
data Linkage = 

setLinkage :: Value -> Linkage -> Buil ()
setLinkage v l = lift $ L.setLinkage v (fromEnum l)
-}

getParam :: Value -> Int -> Value
getParam funcRef = L.getParam funcRef . toEnum

getNamedFunction :: Text -> Build (Maybe Value)
getNamedFunction s = do
  m <- askModule
  debug $ "getNamedFunction: asking for: " ++ show s
  lift $ W.getNamedFunction m (Text.unpack s)

nul :: Type -> Value
nul = L.constNull

getFirstGlobal :: Build Value
getFirstGlobal = askModule >>= lift . L.getFirstGlobal

getNextGlobal :: Value -> Build Value
getNextGlobal = lift . L.getNextGlobal 

getNamedGlobal :: Text -> Build (Maybe Value)
getNamedGlobal str = do
  m <- askModule
  lift $ W.getNamedGlobal m (Text.unpack str)

setValueName :: Value -> String -> Build ()
setValueName v str = lift $ withCString str $ L.setValueName v

getValueName :: Value -> Build String
getValueName v = lift $ L.getValueName v >>= peekCString 

globalString :: String -> Build Value
globalString str =
  let strName = str ++ "_global"
  in withBuilder2 W.buildGlobalStringPtr str strName

string :: String -> Build Value
string origStr = 
   let 
       str = origStr -- ++ "\0"
       strGlob = origStr ++ "_global"
       l = fromIntegral (length str)
   in do
     fmt <- lift $ withCString str ( \ cstr -> return $ L.constString cstr l False)
     t <- typeOfVal fmt
     m <- askModule
     g <- lift $ W.addGlobal m t strGlob
     lift $ L.setInitializer g fmt
     setLinkage g 5 -- WeakAnyLinkage

     return g

struct :: Bool -> [Value] -> Build Value
struct packed elems =
    let cStruct ar = L.constStruct ar (fromIntegral $ length elems) packed
    in lift $ withPtrArray elems (return . cStruct)

load :: Value -> String -> Build Value
load = withBuilder2 W.buildLoad

store :: Value -> Value -> Build Value
store = withBuilder2 W.buildStore

call :: Text -> [Value] -> Build Value
call s args = do
  funcRef <- lookupEnv s
  call' funcRef args (Text.unpack s)

callByName :: Text -> [Value] -> Build (Maybe Value)
callByName f args = do
  fPtrMb <- getNamedFunction f
  case fPtrMb of
    Just fPtr -> Just <$> call' fPtr args ("callByName: " ++ show f)
    Nothing -> return Nothing

-- FIXME: return Void functions can't have a name 
-- However, we can't even examine the return type of the function value
-- because it is reported as a pointer-type, not a function-type!
call' :: Value -> [Value] -> String -> Build Value
call' = withBuilder3 W.buildCall

withArrayLenC ::  Storable a => [a] -> (Ptr a -> CUInt -> IO b) -> IO b
withArrayLenC xs f = withArrayLen xs (\ i p -> f p (fromIntegral i))

invoke :: Text -> [Value] -> BasicBlock -> BasicBlock -> 
          Build Value
invoke s args b1 b2 = do
  f <- lookupEnv s
  invoke' f args b1 b2 (Text.unpack s)

invoke' :: Value -> [Value] -> BasicBlock -> BasicBlock -> 
          String -> Build Value
invoke' f args norm excp _ = 
    withBuilder0 
    (\ b -> withCString "" ( \ s ->
            withArrayLenC args 
              (\ p i -> L.buildInvoke b f p i norm excp s))
    )

buildLandingPad :: Type -> Value -> Int -> String -> Build Value
buildLandingPad typ personality numClauses name =
    withBuilder0 $ \b -> withCString name $ \ cstr ->
      L.buildLandingPad b typ personality (fromIntegral numClauses) cstr

addClause :: Value -> Value -> Build ()
addClause = liftBuild2 L.addClause

setCleanup :: Value -> Bool -> Build ()
setCleanup landingpad cleanup = lift $ L.setCleanup landingpad (fromIntegral $ fromEnum $ cleanup)

alloca :: Type -> Text-> Build Value
alloca tr str = (withBuilder2 W.buildAlloca) tr (Text.unpack str)

unreachable :: Build Value
unreachable = withBuilder0 W.buildUnreachable

retVoid :: Build Value
retVoid = withBuilder0 W.buildRetVoid

ret :: Value -> Build Value
ret = withBuilder1 W.buildRet

icmp :: IntPredicate -> Value -> Value -> String -> Build Value
icmp = withBuilder4 W.buildICmp

fcmp :: FPPredicate -> Value -> Value -> String -> Build Value
fcmp = withBuilder4 W.buildFCmp

siToFP :: Value -> Type -> String -> Build Value
siToFP = withBuilder3 W.buildSIToFP

bitcast :: Value -> Type -> String -> Build Value
bitcast = withBuilder3 W.buildBitCast


addFunc :: Text -> Type -> Build Value
addFunc str typ = do
  m <- askModule
  lift (W.addFunction m  (Text.unpack str) typ)

condBr :: Value -> BasicBlock -> BasicBlock -> Build Value
condBr = withBuilder3 W.buildCondBr

br :: BasicBlock -> Build Value
br = withBuilder1 W.buildBr

positionAtEnd :: BasicBlock -> Build ()
positionAtEnd = withBuilder1 W.positionAtEnd

getInsertBlock :: Build Value
getInsertBlock = withBuilder0 W.getInsertBlock

opWrap :: (Builder -> Value -> Value -> CString -> IO Value) ->
      Value -> Value -> String -> Build Value
opWrap op v1 v2 str = 
    askBuild >>= lift . withCString str . (\ f -> f v1 v2) . op

unOpWrap :: (Builder -> Value -> CString -> IO Value) ->
      Value -> String -> Build Value
unOpWrap op v str = 
    askBuild >>= lift . withCString str . ($ v) . op

add,sub,orr,mul,fdiv :: Value -> Value -> String -> Build Value
add = withBuilder3 W.buildAdd
sub = withBuilder3 W.buildSub
orr = withBuilder3 W.buildOr
andd = withBuilder3 W.buildAnd
mul = withBuilder3 W.buildMul
fdiv = withBuilder3 W.buildFDiv
sdiv = withBuilder3 W.buildSDiv
srem = withBuilder3 W.buildSRem

nott, neg :: Value -> String -> Build Value
nott = withBuilder2 W.buildNot
neg = withBuilder2 W.buildNeg
fneg = withBuilder2 W.buildFNeg


sext :: Value -> Type -> String -> Build Value
sext = withBuilder3 W.buildSExt
  
trunc :: Value -> Type -> String -> Build Value
trunc = withBuilder3 W.buildTrunc
  


getEntryBasicBlock :: Value -> Build BasicBlock
getEntryBasicBlock = liftBuild1 L.getEntryBasicBlock

appendBasicBlock :: Value -> Text -> Build BasicBlock
appendBasicBlock v str = 
    (withContext2 W.appendBasicBlockInContext) v (Text.unpack str)

getBasicBlockParent :: BasicBlock ->  Build Value
getBasicBlockParent = liftBuild1 L.getBasicBlockParent

funcType :: Type -> [Type] -> Build Type
funcType rType argTypes = functionType' rType argTypes False

funcTypeVar :: Type -> [Type] -> Build Type
funcTypeVar rType argTypes = functionType' rType argTypes True

functionType' :: Type -> [Type] -> Bool -> Build Type
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

gep :: Value -> [Value] -> Build Value
gep v idxs = lift (W.constGEP v idxs)

gepInt :: Value -> [Int] -> Build Value
gepInt v is = do
  i32 <- int32TypeM
  gep v $ map (\ i -> W.constInt i32 (fromIntegral i) False) is
