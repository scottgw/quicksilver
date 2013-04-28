{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.QuickSilver.Generate.Feature (genRoutines) where

import Control.Applicative ((<$>))
import Control.Monad
import Control.Monad.Reader

import Data.Maybe (listToMaybe)
import Data.HashMap.Strict (unions, union, empty)
import Data.Text (Text)
import qualified Data.Text as Text

import Language.QuickSilver.Position
import Language.QuickSilver.Syntax hiding (ResultVar)
import Language.QuickSilver.Util
import Language.QuickSilver.TypeCheck.TypedExpr

import Language.QuickSilver.Generate.Eval
-- import Language.QuickSilver.Generate.Builtin.Builtins
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

routineEnv :: TRoutine -> ValueRef -> Build Env
routineEnv rout func = unions <$> sequence [ routResult rout
                                           , genDecls rout
                                           , allocPs func (routineArgs rout)]

lookupLocal :: Text -> Build ValueRef
lookupLocal = (flip load) "" <=< lookupEnv

firstArgTyp :: TRoutine -> Typ
firstArgTyp = maybe (error "firstArgTyp") declType . listToMaybe . routineArgs

genBody :: TRoutine -> Build ()
genBody r =
  case routineImpl r of
    RoutineBody _ _ body        -> genStmt (contents body)
--    RoutineExternal "built_in" _ -> genBuiltin r
    b                           -> error ("genBody: " ++ show b)

start :: TRoutine -> Build ()
start r = getInsertBlock >>= positionAtEnd >> genBody r

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

genRoutine :: TRoutine -> Build ()
genRoutine rout = do
  let ClassType cname _ = firstArgTyp rout
  funcRef <- getNamedFunction (fullNameStr cname (routineName rout))
  setFunctionCallConv funcRef Fast

  routStartBlock rout funcRef
  env <- routineEnv rout funcRef
  debug $ concat [ "Starting to generate "
                 , Text.unpack (routineName rout)
                 , ","
                 , show funcRef
                 ]
  withUpdEnv (union env) (local (setRoutine (makeRoutineI $ untypeRout rout))
                              (preCond rout >> start rout >> routReturn rout))

routStartBlock :: AbsRoutine body exp -> ValueRef -> Build ()
routStartBlock (AbsRoutine {routineName = fName}) funcRef =
  appendBasicBlock funcRef (fName `Text.append` "StartB") >>= positionAtEnd

routResult :: AbsRoutine body exp -> Build Env
routResult (AbsRoutine {routineResult = NoType}) = return empty
routResult f = allocDeclEnv . Decl "Result" . routineResult $ f

routReturn :: TRoutine -> Build ()
routReturn rout =
  case routineImpl rout of
    RoutineExternal _ _ -> return ()
    _                   -> 
      case routineResult rout of
        NoType -> retVoid
        t      -> evalUnPos (ResultVar t) >>= (`load` "") >>= ret

genRoutines :: [TRoutine] -> Build ()
genRoutines = mapM_ genRoutine

