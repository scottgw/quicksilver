{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.QuickSilver.Generate.Routine (genRoutines) where

import Control.Applicative ((<$>))
import Control.Lens
import Control.Monad
import Control.Monad.Reader

import Data.HashMap.Strict (unions, union, empty)
import qualified Data.Text as Text

import Language.QuickSilver.Position
import Language.QuickSilver.Syntax hiding (ResultVar)
import Language.QuickSilver.Util
import Language.QuickSilver.TypeCheck.TypedExpr
import Language.QuickSilver.Generate.Eval
import Language.QuickSilver.Generate.LibQs
import Language.QuickSilver.Generate.Memory.Attribute
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
    RoutineBody locals _ _ -> unions <$> mapM allocDeclEnv locals'
      where
        locals'
           | routHasNonSpecialCurrent r =
               Decl "<CurrentProc>" ProcessorType : locals
           | otherwise = locals
    _ -> return empty

allocP :: ValueRef -> Decl -> Int -> Build Env
allocP fRef d i = do
  debug "Routine: allocating decl for arg"
  vr <- allocDecl d
  debug "Routine: storing parameter from function ref"
  debugDump fRef
  store (getParam fRef i) vr
  debug "Routine: returning new env"
  return (singleEnv d vr)

allocPs :: ValueRef -> [Decl] -> Build Env
allocPs fRef ds = unions `fmap` zipWithM (allocP fRef) ds [0..]

-- genProcDecl :: Build Env
-- genProcDecl =
--     do procType <- procTypeM
--        ref <- alloca procType "<CurrentProc>"
--        return (singleEnv' "<CurrentProc>" ref)

routHasCurrent :: TRoutine -> Bool
routHasCurrent rout =
    case routineArgs rout of
      Decl "Current" _ : _ -> True
      _ -> False

routineEnv :: TRoutine -> ValueRef -> Build Env
routineEnv rout func = 
    unions <$> sequence [ debug "Routine: generating result" >>
                                routResult rout
                        , debug "Routine: generating decls" >> genDecls rout
                        , debug "Routine: genrating args" >>
                                allocPs func (routineArgs rout)
                        -- , debug "Routine: processor" >> genProcDecl
                        ]

routHasNonSpecialCurrent rout =
    case routineArgs rout of
      Decl "Current" t : _ -> not (isSpecialClassName (classNameType t))
      _ -> False

genBody :: TRoutine -> Build ()
genBody r =
  case routineImpl r of
    RoutineBody _ _ body ->
        do when (routHasNonSpecialCurrent r) $
                do currRef <- lookupEnv "Current"
                   curr <- load currRef "loading current to get processor"
                   procRef <- gepInt curr [0, 0]
                   proc <- load procRef "loading processor"
                   procLoc <- lookupEnv "<CurrentProc>"
                   store proc procLoc
                   return ()
           genStmt (contents body)
    RoutineExternal name _ -> genExternal name

genExternal name =
  do debug "generating external"
     func <- getInsertBlock >>= getBasicBlockParent
     let params = countParams func
         args = map (getParam func) [0 .. params - 1]
     debugDump func
     
     mapM_ debugDump args

     externFunc <- getNamedFunction name
     
     v <- call' externFunc args "forwarding to external"
     debug "generated external call"
     debugDump v
     resultMb <- lookupEnvM "Result"
     case resultMb of
       Nothing -> return ()
       Just result -> void (store v result)


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
  clas <- currentClass
  let cname = view className clas
  funcRef <- getNamedFunction (fullNameStr cname (routineName rout))
  debug "Setting calling convention on function"
  -- setFunctionCallConv funcRef Fast
  debug "Creating start block"
  routStartBlock rout funcRef
  debug "Routine: generating environment"
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
  case routineResult rout of
    NoType -> retVoid
    t      -> evalUnPos (ResultVar t) >>= (`load` "") >>= ret

genRoutines :: [TRoutine] -> Build ()
genRoutines = mapM_ genRoutine

