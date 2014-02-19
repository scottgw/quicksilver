{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.QuickSilver.Generate.Routine (genRoutines) where

import Control.Applicative ((<$>))
import Control.Lens
import Control.Monad
import Control.Monad.Reader

import Data.HashMap.Strict (unions, union, empty, fromList)
import qualified Data.Text as Text

import Language.QuickSilver.Position
import Language.QuickSilver.Syntax hiding (ResultVar)
import Language.QuickSilver.Util
import Language.QuickSilver.TypeCheck.TypedExpr
import Language.QuickSilver.Generate.LibQs
import Language.QuickSilver.Generate.Eval
import Language.QuickSilver.Generate.Memory.Types
import Language.QuickSilver.Generate.Statement
import Language.QuickSilver.Generate.LLVM.Simple
import Language.QuickSilver.Generate.LLVM.Build

allocDecl :: Decl -> Build Value
allocDecl (Decl n t) =
    do llvmType <- typeOfM t
       v <- alloca llvmType n

       -- Non-basic types should be marked as GC roots
       when (not $ isBasic t) $
            do charPtrPtr <- pointer0 <$> ptr
               int0 <- int 0
               ptrType <- ptr
               nullPtr <- intToPtr int0 ptrType ""
               gcRootPtr <- bitcast v charPtrPtr ""
               "llvm.gcroot" <#> [gcRootPtr, nullPtr]
               return ()

       return v

allocDeclEnv :: Decl -> Build Env
allocDeclEnv d = singleEnv d `fmap` allocDecl d

genDecls :: TRoutine -> Build Env
genDecls r = 
  case routineImpl r of 
    RoutineBody locals _ -> unions <$> mapM allocDeclEnv locals
    _ -> return empty

allocP :: Value -> Decl -> Int -> Build Env
allocP fRef d i = do
  debug "Routine: allocating decl for arg"
  vr <- allocDecl d
  debug "Routine: storing parameter from function ref"
  store (getParam fRef i) vr
  debugDump fRef
  debug "Routine: returning new env"
  return (singleEnv d vr)

allocPs :: Value -> [Decl] -> Build Env
allocPs fRef ds = unions `fmap` zipWithM (allocP fRef) ds [0..]

genClosureArgs :: Build Env
genClosureArgs =
    do argArrayType <- pointer0 <$> pointer0 <$> voidPtrType
       argArrayPtrRef <- alloca argArrayType "<args>"
       
       argTypesType <- pointer0 <$> closTypeTypeM
       argTypeArrayRef <- alloca argTypesType "<argTypes>"

       i64 <- int64TypeM
       closResult64Ref <- alloca i64 "<closResult64>"
       i32 <- int32TypeM
       closResult32Ref <- alloca i32 "<closResult32>"
       i8 <- int8TypeM
       closResult8Ref <- alloca i8 "<closResult8>"
       i1 <- int1TypeM
       closResult1Ref <- alloca i1 "<closResult1>"


       
       return (fromList [("<args>", argArrayPtrRef)
                        ,("<argTypes>", argTypeArrayRef)
                        ,("<closResult64>", closResult64Ref)
                        ,("<closResult32>", closResult32Ref)
                        ,("<closResult8>", closResult8Ref)
                        ,("<closResult1>", closResult1Ref)
                        ]
              )

routineEnv :: TRoutine -> Value -> Build Env
routineEnv rout func = 
    unions <$> sequence [ debug "Routine: generating result" >>
                                routResult rout
                        , debug "Routine: generating decls" >> genDecls rout
                        , debug "Routine: genrating args" >>
                                allocPs func (routineArgs rout)
                        , debug "Routine: closure args and arg types" >> genClosureArgs
                        ]

genBody :: TRoutine -> Build ()
genBody r =
  case routineImpl r of
    RoutineBody _ body -> genStmt (contents body)
    RoutineExternal name _ -> genExternal name

genExternal name =
  do debug "generating external"
     func <- getInsertBlock >>= getBasicBlockParent
     let params = countParams func
         args = map (getParam func) [0 .. params - 1]
     debugDump func
     
     mapM_ debugDump args

     Just externFunc <- getNamedFunction name
     
     v <- call externFunc args
     debug "generated external call"
     debugDump v
     resultMb <- lookupEnvM "Result"
     case resultMb of
       Nothing -> return ()
       Just result -> void (store v result)


start :: TRoutine -> Build ()
start r = getInsertBlock >>= positionAtEnd >> genBody r

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
  excp <- call' "__cxa_allocate_exception" [int4]
  i8 <- int8TypeM
  i32 <- int32TypeM
  vd  <- voidType
  intptr <- bitcast excp (pointer0 i32) "excpCast"
  int32 <- int 32
  store int32 intptr
  
  let voidF = funcType vd [pointer0 i8]
  stdType <- lookupEnv "_ZTIi"
  castType <- bitcast stdType (pointer0 i8) "stdTypeCast"

  call' "__cxa_throw" [excp, castType, nul (pointer0 voidF)]
  return ()

preCond :: TRoutine -> Build ()
preCond = mapM_ clause . contractClauses . routineReq

genRoutine :: Bool -> TRoutine -> Build ()
genRoutine isMain rout = do
  clas <- currentClass
  let cname = view className clas
  Just funcRef <- getNamedFunction (fullNameStr cname (routineName rout))
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
                          (do preCond rout
                              start rout
                              routReturn rout
                          )
                         )

routStartBlock :: AbsRoutine body exp -> Value -> Build ()
routStartBlock (AbsRoutine {routineName = fName}) funcRef =
  appendBasicBlock funcRef (fName `Text.append` "StartB") >>= positionAtEnd

routResult :: AbsRoutine body exp -> Build Env
routResult (AbsRoutine {routineResult = NoType}) = return empty
routResult f = allocDeclEnv . Decl "Result" . routineResult $ f

routReturn :: TRoutine -> Build ()
routReturn rout =
  case routineResult rout of
    NoType -> void retVoid
    t      -> evalUnPos (ResultVar t) >>= void . ret

genRoutines :: Bool -> [TRoutine] -> Build ()
genRoutines isMain = mapM_ (genRoutine isMain)

