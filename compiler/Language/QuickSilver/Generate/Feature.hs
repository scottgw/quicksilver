{-# LANGUAGE ViewPatterns #-}
module Language.QuickSilver.Generate.Feature (genFeatures) where

import Control.Applicative ((<$>))
import Control.Monad
import Control.Monad.Reader

import Data.Maybe (listToMaybe)
import Data.Map (unions, union, empty)

import Language.QuickSilver.Position
import Language.QuickSilver.Syntax hiding (ResultVar)
import Language.QuickSilver.Util
import Language.QuickSilver.TypeCheck.TypedExpr

import Language.QuickSilver.Generate.Eval
import Language.QuickSilver.Generate.Builtin.Builtins
import Language.QuickSilver.Generate.Memory.Attribute
import Language.QuickSilver.Generate.Memory.Class
import Language.QuickSilver.Generate.Statement

import Language.QuickSilver.Generate.LLVM.Simple
import Language.QuickSilver.Generate.LLVM.Types
import Language.QuickSilver.Generate.LLVM.Util

allocDecl :: Decl -> Build ValueRef
allocDecl (Decl n t) = typeOfM t >>= flip alloca n

allocDeclEnv :: Decl -> Build Env
allocDeclEnv d = singleEnv d `fmap` allocDecl d

genDecls :: TRoutine -> Build Env
genDecls r = 
  case routineImpl r of 
    RoutineBody locals _ _ -> unions <$> mapM allocDeclEnv locals
    _ -> return empty

allocP :: ValueRef -> Decl -> Int -> Build Env
allocP fRef d i = do
  vr <- allocDecl d
  store (getParam fRef i) vr >> return (singleEnv d vr)

allocPs :: ValueRef -> [Decl] -> Build Env
allocPs fRef ds = unions `fmap` zipWithM (allocP fRef) ds [0..]

featureEnv :: TRoutine -> ValueRef -> Build Env
featureEnv feat func = unions <$> sequence [ featResult feat
                                           , genDecls feat
                                           , allocPs func (featureArgs feat)]

lookupLocal :: String -> Build ValueRef
lookupLocal = (flip load) "" <=< lookupEnv

firstArgTyp :: TRoutine -> Typ
firstArgTyp = maybe (error "firstArgTyp") declType . listToMaybe . featureArgs

start :: TRoutine -> Build ()
start f = if routineFroz f 
          then frozenStart f 
          else virtualStart f

genBody :: TRoutine -> Build ()
genBody r =
  case routineImpl r of
    RoutineBody _ _ body        -> genStmt (contents body)
    RoutineExternal "built_in" _ -> genBuiltin r
    b                           -> error ("genBody: " ++ show b)

frozenStart :: TRoutine -> Build ()
frozenStart r = getInsertBlock >>= positionAtEnd >> genBody r

virtualStart :: TRoutine -> Build ()
virtualStart feat = do
  func     <- getInsertBlock >>= getBasicBlockParent
  normalB  <- appendBasicBlock func "normal"
  vtableB  <- appendBasicBlock func "vtable"
  mergeB   <- appendBasicBlock func "merge"

  let ClassType cName _ = firstArgTyp feat
  vt     <- lookupVTable cName
  clas   <- lookupClas cName
  obj    <- lookupLocal "Current"

  vtable <- getVTable cName obj 

  i64    <- int64TypeM
  vtPtr1 <- ptrToInt vt i64 ""
  vtPtr2 <- ptrToInt vtable i64 ""

  res  <- icmp IntEQ vtPtr1 vtPtr2 ""
  condBr res normalB vtableB

  positionAtEnd normalB
  genBody feat
  br mergeB
  
  positionAtEnd vtableB
  f <- vtableFunc clas obj (featureName feat)
  args <- mapM lookupLocal (map declName . featureArgs $ feat)
  r <- call' f args "vtableBlockCall"
  setInstructionCallConv r Fast
  br mergeB

  positionAtEnd mergeB

evalClause :: Clause TExpr -> Build ValueRef
evalClause (Clause _n e) = loadEval e

clause :: Clause TExpr -> Build ()
clause c = do
  func <- getInsertBlock >>= getBasicBlockParent
  cont <- appendBasicBlock func "continue"
  exit <- appendBasicBlock func "exit"

  tr   <- true
  b <- evalClause c >>= flip (icmp IntEQ tr) "precond branch"
  condBr b cont exit

  positionAtEnd exit
  throw
  unreachable
  
  positionAtEnd cont

throw :: Build ()
throw = do
  int4 <- int 4
  excp <- call "__cxa_allocate_exception" [int4]
  i8 <- int8TypeM
  i32 <- int32TypeM
  vd  <- voidTypeM
  intptr <- bitcast excp (pointer0 i32) "excpCast"
  int32 <- int 32
  store int32 intptr
  
  voidF <- funcType vd [pointer0 i8]
  stdType <- lookupEnv "_ZTIi"
  castType <- bitcast stdType (pointer0 i8) "stdTypeCast"

  call "__cxa_throw" [excp, castType, nul (pointer0 voidF)]
  return ()

preCond :: TRoutine -> Build ()
preCond = mapM_ clause . contractClauses . routineReq

genFeature :: TRoutine -> Build ()
genFeature feat = do
  let ClassType cname _ = firstArgTyp feat
  funcRef <- getNamedFunction (fullNameStr cname (featureName feat))
  setFunctionCallConv funcRef Fast

  featStartBlock feat funcRef
  env <- featureEnv feat funcRef
  debug $ "Starting to generate " ++ featureName feat ++ "," ++ show funcRef
  withUpdEnv (union env) (local (setFeature (makeRoutineI $ untypeFeat feat))
                              (preCond feat >> start feat >> featReturn feat))

featStartBlock :: AbsRoutine body exp -> ValueRef -> Build ()
featStartBlock (AbsRoutine {routineName = fName}) funcRef =
  appendBasicBlock funcRef (fName ++ "StartB") >>= positionAtEnd

featResult :: AbsRoutine body exp -> Build Env
featResult (AbsRoutine {routineResult = NoType}) = return empty
featResult f = allocDeclEnv . Decl "Result" . routineResult $ f

featReturn :: TRoutine -> Build ()
featReturn rout =
  case routineImpl rout of
    RoutineExternal _ _ -> return ()
    _                   -> 
      case routineResult rout of
        NoType -> retVoid
        t      -> evalUnPos (ResultVar t) >>= (`load` "") >>= ret

genFeatures :: [TRoutine] -> Build ()
genFeatures = mapM_ genFeature

