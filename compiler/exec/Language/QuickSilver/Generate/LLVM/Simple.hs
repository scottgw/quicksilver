{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Language.QuickSilver.Generate.LLVM.Simple
    ( runLLVMReader
    , LLVM (..)
    , LLVMData (..)
    , LLVMReaderT
    , LLVMReader

    , Context
    , Value
    , Type
    , Builder
    , Module
    , dumpModule
    , dumpValue

      -- ** Globals
    , CallingConvention (..)
    , Linkage (..)
    , Visibility (..)
    , addGlobal
    , setInitializer
    , setGlobalConstant
    , getNamedGlobal
    , getFirstGlobal
    , getNextGlobal
    , setVisibility
    , getVisibility
    , setLinkage
    , getLinkage
    , string
    , globalString

      -- ** Operators
    , IntPredicate (..)
    , FPPredicate (..)
    , constInt
    , constReal
    , add
    , fadd
    , sub
    , fsub
    , mul
    , fmul
    , fdiv
    , neg
    , fneg
    , sdiv
    , srem
    , nott
    , andd
    , orr
    , icmp
    , fcmp  
    , sext
    , trunc
    , siToFP

      -- ** Control flow
    , ret
    , retVoid
    , condBr
    , br
    , call
    , callByName
    , invoke
    , unreachable

      -- ** Landing pad
    , buildLandingPad
    , setCleanup
    , addClause

      -- ** Memory
    , alloca
    , load
    , store
    , nul
    , ptrToInt
    , intToPtr
    , gep
    , bitcast
    , constPtrToInt

      -- ** Basic blocks
    , BasicBlock
    , getInsertBlock
    , getEntryBasicBlock
    , appendBasicBlock
    , positionAtEnd
    , getBasicBlockParent

      -- ** Phi node
    , phi
    , addIncoming

      -- ** Types
    , TypeKind(..)
    , intType
    , doubleType
    , voidType
    , arrayType
    , struct
    , structType
    , structCreateNamed
    , structSetBody
    , countStructElementTypes
    , getTypeKind
    , getTypeByName
    , getElementType
    , functionType

      -- ** Functions      
    , addFunction
    , getNamedFunction
    , countParamTypes
    , getParamTypes
    , countParams
    , getParam
    , setFunctionCallConv
    , setInstructionCallConv
    ) where

import           Control.Applicative
import           Control.Monad.Reader

import           Data.Text (Text)
import qualified Data.Text as Text

import qualified LLVM.Wrapper.Core as W 
import           LLVM.Wrapper.Core ( Builder, Context, Module
                                   , Value, BasicBlock, Type, TypeKind
                                   , CallingConvention, FPPredicate
                                   , IntPredicate
                                   , Visibility
                                   , Linkage
                                   , constInt, constReal, constPtrToInt
                                   )

class (Functor m, MonadIO m) => LLVM m where
  askBuild :: m Builder
  askContext :: m Context
  askModule :: m Module

data LLVMData =
  LLVMData { llvmBuild :: Builder
           , llvmContext :: Context
           , llvmModule :: Module
           }

type LLVMReaderT m = ReaderT LLVMData m
newtype LLVMReader a = LLVMReader { unLLVMReader :: LLVMReaderT IO a }

runLLVMReader :: LLVMReader a -> LLVMData -> IO a
runLLVMReader (LLVMReader m) = runReaderT m 

instance Applicative LLVMReader where
  pure x   = return x
  (LLVMReader f) <*> (LLVMReader m) = LLVMReader (f <*> m)

instance Functor LLVMReader where
  fmap f (LLVMReader m) = LLVMReader (fmap f m)

instance Monad LLVMReader where
  (LLVMReader m) >>= f = LLVMReader (m >>= (unLLVMReader . f))
  return a = LLVMReader (return a)

instance MonadIO LLVMReader where
  liftIO m = LLVMReader (liftIO m)

instance LLVM LLVMReader where
  askBuild = llvmBuild <$> (LLVMReader ask)
  askContext = llvmContext <$> (LLVMReader ask)
  askModule = llvmModule <$> (LLVMReader ask)

instance LLVM (ReaderT r LLVMReader) where
  askBuild = lift askBuild
  askContext = lift askContext
  askModule = lift askModule

dumpModule :: LLVM m => m ()
dumpModule = askModule >>= liftIO . W.dumpModule

setGlobalConstant :: LLVM m => Value -> Bool -> m ()
setGlobalConstant v b = liftIO (W.setGlobalConstant v b)

setVisibility :: LLVM m => Value -> Visibility -> m ()
setVisibility v vis = liftIO $ W.setVisibility v vis

getVisibility :: LLVM m => Value -> m Visibility
getVisibility = liftIO . W.getVisibility

setLinkage :: LLVM m => Value -> Linkage -> m ()
setLinkage v link = liftIO $ W.setLinkage v link

getLinkage :: LLVM m => Value -> m Linkage
getLinkage = liftIO . W.getLinkage

addGlobal :: LLVM m => Type -> String -> m Value
addGlobal t s = do
    m <- askModule
    liftIO $ W.addGlobal m t s

setInitializer :: LLVM m => Value -> Value -> m ()
setInitializer = liftBuild2  W.setInitializer

setFunctionCallConv :: LLVM m => Value -> CallingConvention -> m ()
setFunctionCallConv = liftBuild2 W.setFunctionCallConv

setInstructionCallConv :: LLVM m => Value -> CallingConvention -> m ()
setInstructionCallConv = liftBuild2 W.setInstructionCallConv 

dumpValue :: LLVM m => Value -> m ()
dumpValue = liftBuild1 W.dumpValue

ptrToInt :: LLVM m => Value -> Type -> String -> m Value
ptrToInt = withBuilder3 W.buildPtrToInt

intToPtr :: LLVM m => Value -> Type -> String -> m Value
intToPtr = withBuilder3 W.buildIntToPtr

countParamTypes :: Type -> Int
countParamTypes = W.countParamTypes

countParams :: Value -> Int
countParams = W.countParams

getParamTypes :: Type -> [Type]
getParamTypes = W.getParamTypes

getParam :: Value -> Int -> Value
getParam = W.getParam

getNamedFunction :: LLVM m => Text -> m (Maybe Value)
getNamedFunction s = do
  m <- askModule
  liftIO $ W.getNamedFunction m (Text.unpack s)

nul :: Type -> Value
nul = W.constNull

getFirstGlobal :: LLVM m => m Value
getFirstGlobal = askModule >>= liftIO . W.getFirstGlobal

getNextGlobal :: LLVM m => Value -> m Value
getNextGlobal = liftIO . W.getNextGlobal 

getNamedGlobal :: LLVM m => Text -> m (Maybe Value)
getNamedGlobal str = do
  m <- askModule
  liftIO $ W.getNamedGlobal m (Text.unpack str)

globalString :: LLVM m => String -> m Value
globalString str =
  let strName = str ++ "_global"
  in withBuilder2 W.buildGlobalStringPtr str strName

string :: LLVM m => String -> m Value
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

struct :: LLVM m => [Value] -> Bool -> m Value
struct = withContext2 W.constStructInContext

load :: LLVM m => Value -> String -> m Value
load = withBuilder2 W.buildLoad

store :: LLVM m => Value -> Value -> m Value
store = withBuilder2 W.buildStore

callByName :: LLVM m => Text -> [Value] -> m (Maybe Value)
callByName f args = do
  fPtrMb <- getNamedFunction f
  case fPtrMb of
    Just fPtr -> Just <$> call fPtr args
    Nothing -> return Nothing

-- FIXME: return Void functions can't have a name 
-- However, we can't even examine the return type of the function value
-- because it is reported as a pointer-type, not a function-type!
call :: LLVM m => Value -> [Value] -> m Value
call fn args = withBuilder3 W.buildCall fn args ""

invoke :: LLVM m => Value -> [Value] -> BasicBlock -> BasicBlock -> 
          String -> m Value
invoke = withBuilder5 W.buildInvoke

buildLandingPad :: LLVM m => Type -> Value -> Int -> String -> m Value
buildLandingPad typ personality numClauses name =
    withBuilder0 $ \b ->
      W.buildLandingPad b typ personality (fromIntegral numClauses) name

addClause :: LLVM m => Value -> Value -> m ()
addClause = liftBuild2 W.addClause

setCleanup :: LLVM m => Value -> Bool -> m ()
setCleanup = liftBuild2 W.setCleanup

alloca :: LLVM m => Type -> Text-> m Value
alloca tr str = withBuilder2 W.buildAlloca tr (Text.unpack str)

unreachable :: LLVM m => m Value
unreachable = withBuilder0 W.buildUnreachable

retVoid :: LLVM m => m Value
retVoid = withBuilder0 W.buildRetVoid

ret :: LLVM m => Value -> m Value
ret = withBuilder1 W.buildRet

icmp :: LLVM m => IntPredicate -> Value -> Value -> String -> m Value
icmp = withBuilder4 W.buildICmp

fcmp :: LLVM m => FPPredicate -> Value -> Value -> String -> m Value
fcmp = withBuilder4 W.buildFCmp

siToFP :: LLVM m => Value -> Type -> String -> m Value
siToFP = withBuilder3 W.buildSIToFP

bitcast :: LLVM m => Value -> Type -> String -> m Value
bitcast = withBuilder3 W.buildBitCast


addFunction :: LLVM m => Text -> Type -> m Value
addFunction str typ = do
  m <- askModule
  liftIO (W.addFunction m  (Text.unpack str) typ)

condBr :: LLVM m => Value -> BasicBlock -> BasicBlock -> m Value
condBr = withBuilder3 W.buildCondBr

br :: LLVM m => BasicBlock -> m Value
br = withBuilder1 W.buildBr

positionAtEnd :: LLVM m => BasicBlock -> m ()
positionAtEnd = withBuilder1 W.positionAtEnd

getInsertBlock :: LLVM m => m Value
getInsertBlock = withBuilder0 W.getInsertBlock

add, fadd, sub, fsub, orr, andd, mul, fmul, fdiv, sdiv, srem :: 
   LLVM m => Value -> Value -> String -> m Value
add = withBuilder3 W.buildAdd
fadd = withBuilder3 W.buildFAdd
sub = withBuilder3 W.buildSub
fsub = withBuilder3 W.buildFSub
orr = withBuilder3 W.buildOr
andd = withBuilder3 W.buildAnd
mul = withBuilder3 W.buildMul
fmul = withBuilder3 W.buildFMul
fdiv = withBuilder3 W.buildFDiv
sdiv = withBuilder3 W.buildSDiv
srem = withBuilder3 W.buildSRem

nott, neg, fneg :: LLVM m => Value -> String -> m Value
nott = withBuilder2 W.buildNot
neg = withBuilder2 W.buildNeg
fneg = withBuilder2 W.buildFNeg


sext :: LLVM m => Value -> Type -> String -> m Value
sext = withBuilder3 W.buildSExt
  
trunc :: LLVM m => Value -> Type -> String -> m Value
trunc = withBuilder3 W.buildTrunc

getEntryBasicBlock :: LLVM m => Value -> m BasicBlock
getEntryBasicBlock = liftBuild1 W.getEntryBasicBlock

appendBasicBlock :: LLVM m => Value -> Text -> m BasicBlock
appendBasicBlock v str = 
    (withContext2 W.appendBasicBlockInContext) v (Text.unpack str)

getBasicBlockParent :: LLVM m => BasicBlock ->  m Value
getBasicBlockParent = liftBuild1 W.getBasicBlockParent

phi :: LLVM m => Type -> String -> m Value
phi = withBuilder2 W.buildPhi

addIncoming :: LLVM m => Value -> [(Value, BasicBlock)] -> m ()
addIncoming = liftBuild2 W.addIncoming

gep :: LLVM m => Value -> [Value] -> m Value
gep v idxs = withBuilder0 (\b -> W.buildGEP b v idxs "")

-- ** Types
typeOfVal :: LLVM m => Value -> m Type
typeOfVal = liftBuild1 W.typeOf

structCreateNamed :: LLVM m => Text -> m Type
structCreateNamed str = do
  withContext1 W.structCreateNamedInContext (Text.unpack str)

structSetBody :: LLVM m => Type -> [Type] -> Bool -> m ()
structSetBody = liftBuild3 W.structSetBody

getTypeKind :: LLVM m => Type -> m TypeKind
getTypeKind = liftBuild1 W.getTypeKind

getTypeByName :: LLVM m => Text -> m (Maybe Type)
getTypeByName name = 
    do modul <- askModule
       liftIO (W.getTypeByName modul (Text.unpack name))

getElementType :: LLVM m => Type -> m Type
getElementType = liftBuild1 W.getElementType

structType :: LLVM m => [Type] -> Bool -> m Type
structType = withContext2 W.structTypeInContext

countStructElementTypes :: Type -> Int
countStructElementTypes = W.countStructElementTypes

intType :: LLVM m => Int -> m Type
intType = withContext1 W.intTypeInContext . fromIntegral

doubleType :: LLVM m => m Type
doubleType = withContext0 W.doubleTypeInContext

voidType :: LLVM m => m Type
voidType = withContext0 W.voidTypeInContext

arrayType :: Type -> Int -> Type
arrayType t int  = W.arrayType t (fromIntegral int)

functionType :: Type -> [Type] -> Bool -> Type
functionType = W.functionType


{-
building lifters, just in case
-}

liftBuild1 :: LLVM m => (a -> IO x) -> a -> m x
liftBuild1 f a = liftIO $ f a

liftBuild2 :: LLVM m => (a -> b -> IO x) -> a -> b -> m x
liftBuild2 f a b = liftIO $ f a b

liftBuild3 :: LLVM m =>
              (a -> b -> c -> IO x) -> 
              a -> b -> c -> m x
liftBuild3 f a b c = liftIO $ f a b c

withBuilder0 :: LLVM m => (Builder -> IO x) -> m x
withBuilder0 f = askBuild >>= liftIO . f

withBuilder1 :: LLVM m => (Builder -> a -> IO x) -> a -> m x
withBuilder1 f a = askBuild >>= \ bRef -> liftIO $ f bRef a

withBuilder2 :: LLVM m =>
                (Builder -> a -> b -> IO x) -> 
                a -> b -> m x
withBuilder2 f a b = askBuild >>= \ bRef -> liftIO $ f bRef a b

withBuilder3 :: LLVM m =>
                (Builder -> a -> b -> c -> IO x) -> 
                a -> b -> c -> m x
withBuilder3 f a b c = askBuild >>= \ bRef -> liftIO $ f bRef a b c

withBuilder4 :: LLVM m =>
                (Builder -> a -> b -> c -> d -> IO x) -> 
                a -> b -> c -> d -> m x
withBuilder4 f a b c d = askBuild >>= \ bRef -> liftIO $ f bRef a b c d

withBuilder5 :: LLVM m =>
                (Builder -> a -> b -> c -> d -> e -> IO x) -> 
                a -> b -> c -> d -> e -> m x
withBuilder5 f a b c d e= askBuild >>= \ bRef -> liftIO $ f bRef a b c d e 

withContext0 :: LLVM m => (Context -> IO x) -> m x
withContext0 f = askContext >>= liftIO . f

withContext1 :: LLVM m => (Context -> a -> IO x) -> a -> m x
withContext1 f a = askContext >>= \ bRef -> liftIO $ f bRef a

withContext2 :: LLVM m => (Context -> a -> b -> IO x) -> 
                a -> b -> m x
withContext2 f a b = askContext >>= \ bRef -> liftIO $ f bRef a b
