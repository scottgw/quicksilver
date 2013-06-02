module Language.QuickSilver.Generate.LLVM.Simple
    (
     IntPredicate (..), FPPredicate (..),
     CallingConvention (..), Linkage (..),
     Context, Value, Type, Builder, BasicBlock,
     Build, Env,

     dumpModule,
     askModule, writeModuleToFile, runBuild,
     withUpdEnv, lookupEnv, fromListEnv,

     alloca, load, store,

     addGlobal, setInitializer, setGlobalConstant, getNamedGlobal,
     getFirstGlobal, getNextGlobal,

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

     buildPhi, addIncoming,
     
     globalString, string, nul, constInt, constReal,  struct,

     unreachable,

     buildLandingPad, setCleanup, addClause,

     addFunc, funcType, funcTypeVar, getNamedFunction,
     countParamTypes,
     getParamTypes, countParams, getParam,
     call, call',callByName,
     invoke, invoke',
     setFunctionCallConv,
     setInstructionCallConv
    ) where

import Control.Applicative
import Control.Monad.Reader

import           Data.Text (Text)
import qualified Data.Text as Text

import qualified LLVM.Wrapper.Core as W 
import           LLVM.Wrapper.Core ( Value, BasicBlock, Builder, Context
                                   , CallingConvention, FPPredicate
                                   , IntPredicate
                                   , Visibility
                                   , Linkage
                                   , constInt, constReal, constPtrToInt
                                   , functionType)

import Language.QuickSilver.Generate.LLVM.Types
import Language.QuickSilver.Generate.LLVM.Util


dumpModule :: Build ()
dumpModule = askModule >>= lift . W.dumpModule

setGlobalConstant :: Value -> Bool -> Build ()
setGlobalConstant v b = lift (W.setGlobalConstant v b)

setVisibility :: Value -> Visibility -> Build ()
setVisibility v vis = lift $ W.setVisibility v vis

getVisibility :: Value -> Build Visibility
getVisibility = lift . W.getVisibility

setLinkage :: Value -> Linkage -> Build ()
setLinkage v link = lift $ W.setLinkage v link

getLinkage :: Value -> Build Linkage
getLinkage = lift . W.getLinkage

addGlobal :: Type -> String -> Build Value
addGlobal t s = do
    m <- askModule
    lift $ W.addGlobal m t s

setInitializer :: Value -> Value -> Build ()
setInitializer = liftBuild2  W.setInitializer

setFunctionCallConv :: Value -> CallingConvention -> Build ()
setFunctionCallConv v conv = 
    lift $ W.setFunctionCallConv v conv

setInstructionCallConv :: Value -> CallingConvention -> Build ()
setInstructionCallConv v conv = 
  lift $ W.setInstructionCallConv v conv

dumpValue :: Value -> Build ()
dumpValue v = lift (W.dumpValue v)

ptrToInt :: Value -> Type -> String -> Build Value
ptrToInt = withBuilder3 W.buildPtrToInt

intToPtr :: Value -> Type -> String -> Build Value
intToPtr = withBuilder3 W.buildIntToPtr

countParamTypes :: Type -> Int
countParamTypes = W.countParamTypes

countParams :: Value -> Int
countParams = W.countParams

getParamTypes :: Type -> [Type]
getParamTypes = W.getParamTypes

getParam :: Value -> Int -> Value
getParam = W.getParam

getNamedFunction :: Text -> Build (Maybe Value)
getNamedFunction s = do
  m <- askModule
  debug $ "getNamedFunction: asking for: " ++ show s
  lift $ W.getNamedFunction m (Text.unpack s)

nul :: Type -> Value
nul = W.constNull

getFirstGlobal :: Build Value
getFirstGlobal = askModule >>= lift . W.getFirstGlobal

getNextGlobal :: Value -> Build Value
getNextGlobal = lift . W.getNextGlobal 

getNamedGlobal :: Text -> Build (Maybe Value)
getNamedGlobal str = do
  m <- askModule
  lift $ W.getNamedGlobal m (Text.unpack str)

globalString :: String -> Build Value
globalString str =
  let strName = str ++ "_global"
  in withBuilder2 W.buildGlobalStringPtr str strName

string :: String -> Build Value
string origStr = 
   let 
       str = origStr -- ++ "\0"
       strGlob = origStr ++ "_global"
       fmt = W.constString str False
   in do t <- typeOfVal fmt
         g <- addGlobal t strGlob
         setInitializer g fmt
         setLinkage g W.WeakAnyLinkage
         return g

struct :: [Value] -> Bool -> Build Value
struct = withContext2 W.constStructInContext

load :: Value -> String -> Build Value
load = withBuilder2 W.buildLoad

store :: Value -> Value -> Build Value
store = withBuilder2 W.buildStore

call :: Text -> [Value] -> Build Value
call s args = do
  funcRef <- lookupEnv s
  call' funcRef args

callByName :: Text -> [Value] -> Build (Maybe Value)
callByName f args = do
  fPtrMb <- getNamedFunction f
  case fPtrMb of
    Just fPtr -> Just <$> call' fPtr args
    Nothing -> return Nothing

-- FIXME: return Void functions can't have a name 
-- However, we can't even examine the return type of the function value
-- because it is reported as a pointer-type, not a function-type!
call' :: Value -> [Value] -> Build Value
call' fn args = withBuilder3 W.buildCall fn args ""

invoke :: Text -> [Value] -> BasicBlock -> BasicBlock -> 
          Build Value
invoke s args b1 b2 = do
  f <- lookupEnv s
  invoke' f args b1 b2 (Text.unpack s)

invoke' :: Value -> [Value] -> BasicBlock -> BasicBlock -> 
          String -> Build Value
invoke' f args norm excp str = 
    withBuilder0 (\ b -> W.buildInvoke b f args norm excp str)

buildLandingPad :: Type -> Value -> Int -> String -> Build Value
buildLandingPad typ personality numClauses name =
    withBuilder0 $ \b ->
      W.buildLandingPad b typ personality (fromIntegral numClauses) name

addClause :: Value -> Value -> Build ()
addClause = liftBuild2 W.addClause

setCleanup :: Value -> Bool -> Build ()
setCleanup landingPad cleanup = lift (W.setCleanup landingPad cleanup)

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
getEntryBasicBlock = liftBuild1 W.getEntryBasicBlock

appendBasicBlock :: Value -> Text -> Build BasicBlock
appendBasicBlock v str = 
    (withContext2 W.appendBasicBlockInContext) v (Text.unpack str)

getBasicBlockParent :: BasicBlock ->  Build Value
getBasicBlockParent = liftBuild1 W.getBasicBlockParent

buildPhi :: Type -> String -> Build Value
buildPhi = withBuilder2 W.buildPhi

addIncoming :: Value -> [(Value, BasicBlock)] -> Build ()
addIncoming phi edges = lift (W.addIncoming phi edges)


funcType :: Type -> [Type] -> Type
funcType rType argTypes = functionType rType argTypes False

funcTypeVar :: Type -> [Type] -> Type
funcTypeVar rType argTypes = functionType rType argTypes True

gep :: Value -> [Value] -> Build Value
gep v idxs = withBuilder0 (\b -> W.buildGEP b v idxs "")

gepInt :: Value -> [Int] -> Build Value
gepInt v is = do
  i32 <- int32TypeM
  gep v $ map (\ i -> W.constInt i32 (fromIntegral i) True) is
