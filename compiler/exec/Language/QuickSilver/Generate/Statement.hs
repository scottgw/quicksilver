{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.QuickSilver.Generate.Statement (genStmt) where

import Control.Monad

import Data.Text (Text)

import Language.QuickSilver.Syntax
import Language.QuickSilver.Position
import Language.QuickSilver.TypeCheck.TypedExpr as T
import Language.QuickSilver.Generate.Eval
import Language.QuickSilver.Generate.LibQs
import Language.QuickSilver.Generate.Memory.Object
import Language.QuickSilver.Generate.LLVM.Simple
import Language.QuickSilver.Generate.LLVM.Util

fetchCurrentAttr :: Text -> Build ValueRef
fetchCurrentAttr ident = do
  curr <- lookupEnv  "Current"
  clas <- currentClass
  obj  <- load curr "lookupVarOrAttr load"
  getAttribute clas ident obj

lookupVarAccess :: UnPosTExpr -> Build ValueRef
lookupVarAccess (Var i _) = lookupEnv i
lookupVarAccess (T.ResultVar _) = lookupEnv "Result"
lookupVarAccess (Access _ i _) = fetchCurrentAttr i
lookupVarAccess e = error ("lookupVarAccess: not Var or Access: " ++ show e)

genStmt :: UnPosTStmt -> Build ()
genStmt (CallStmt e)        = eval e >> return ()
genStmt (Block ss)          = mapM_ (genStmt . contents) ss
genStmt (Assign ident expr) = do
  debug $ "Assign to: " ++ show ident

  lhs <- eval ident
  debug "Assign: lhs"
  debugDump lhs
  
  rhs <- loadEval expr
  debug "Assign: rhs"
  debugDump rhs

  debug "Assign: storing"
  store rhs lhs
  return ()
genStmt (If cond then_ elseIfs elseMb) = do
  debug "genStmt: if"
  condV  <- loadEval cond

  startB <- getInsertBlock
  func   <- getBasicBlockParent startB
  thenB  <- appendBasicBlock func "then"
  elseB  <- appendBasicBlock func "else"
  mergeB <- appendBasicBlock func "merge"

  -- Language.QuickSilver.Generate a single elseIf case, using the next elseIf as a branch
  -- destination. Should be used in a fold over all the elseIfParts
  let genIfElse nextIfCase (ElseIfPart c stmt) = do
        elseIfB     <- appendBasicBlock func "elseIf"
        elseIfThenB <- appendBasicBlock func "elseIfThen"
        
        positionAtEnd elseIfB
        condVal <- loadEval c
        _ <- condBr condVal elseIfThenB nextIfCase
    
        positionAtEnd elseIfThenB
        genStmt (contents stmt)
        br mergeB
    
        return elseIfB

  elseIfB <- foldM genIfElse elseB (reverse elseIfs)
  
  positionAtEnd thenB
  genStmt (contents then_)
  br mergeB

  positionAtEnd elseB
  maybe (return ()) (genStmt . contents) elseMb
  br mergeB

  positionAtEnd startB
  condBr condV thenB elseIfB

  positionAtEnd mergeB
  return ()
genStmt (Loop setup _invs cond body _varMb) = do
  startB  <- getInsertBlock
  func    <- getBasicBlockParent startB

  condB   <- appendBasicBlock func "loop condition"
  bodyB   <- appendBasicBlock func "loop body"
  afterB  <- appendBasicBlock func "after loop"

  positionAtEnd startB
  genStmt (contents setup)
  _ <- br condB

  positionAtEnd condB
  res  <- loadEval cond
  _ <- condBr res afterB bodyB

  positionAtEnd bodyB
  genStmt (contents body)
  _ <- br condB

  positionAtEnd afterB
  return ()

genStmt (Separate args body) =
  do privQs <- lockSeps args
     genStmt (contents body)
     unlockQueues privQs
  
genStmt (Create _typeMb var fName args) =
  case texpr var of
    varType@(Sep _ _ name) ->
      do debug "creating separate object"
         varRef <- lookupVarAccess (contents var)
         newSep <- mallocSeparate name
         debugDump newSep

         currProc <- getCurrProc
         newProc <- "make_processor_from" <#> [currProc]

         newInst <- lookupMalloc (ClassType name []) fName args

         debug "creating separate: storing new proc"
         procLoc <- gepInt newSep [0, 0]
         store newProc procLoc

         debug "creating separate: storing base class"
         instLoc <- gepInt newSep [0, 1]
         voidPtr <- voidPtrType
         newInst' <- bitcast newInst voidPtr "voidPtrInstance"
         store newInst' instLoc
         
         store newSep varRef

         genStmt (CallStmt $
                  attachPos (position var) (Call var fName args NoType)
                 )
    varType ->     
      do varRef <- lookupVarAccess (contents var)
         newInst <- lookupMalloc varType fName args
         store newInst varRef
         genStmt (CallStmt $
                  attachPos (position var) (Call var fName args NoType)
                 )
         return ()
-- genStmt BuiltIn = lookupBuiltin
genStmt s = error $ "genStmt: no pattern for: " ++ show s

lockSeps :: [TExpr] -> Build [ValueRef]
lockSeps = mapM lockSep
  where
    lockSep e@(contents -> Var _name _t) =
      do debug "lockSep"
         e' <- loadEval e
         debugDump e'
         currProc <- getCurrProc
         prc <- getProc e'
         debugDump prc
         privQ <- "priv_queue_new" <#> [prc]
         "priv_queue_lock" <#> [privQ, prc, currProc]
         return privQ
    lockSep _ = error "lockSep: found non-variable expression"

unlockQueues :: [ValueRef] -> Build ()
unlockQueues = mapM_ unlockQueue
  where
    unlockQueue privQ =
      do debug "unlockQueues"
         currProc <- getCurrProc
         "priv_queue_unlock" <#> [privQ, currProc]


-- lookupBuiltin :: Build ()
-- lookupBuiltin = do
--   fName <- featureName `fmap` currentFeature
--   cName <- className `fmap` currentClass
--   genBuiltin cName fName

lookupMalloc :: Typ -> Text -> [TExpr] -> Build ValueRef
lookupMalloc t  _fName _args =
  case t of
    ClassType cName _ -> mallocObject cName
    -- FIXME: Add separate wrapper datatype
    Sep _ _ cName -> mallocObject cName
    _ -> error ("lookupMalloc: called on non-class type: " ++ show t)

