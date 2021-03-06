{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.QuickSilver.Generate.Statement (genStmt) where

import Control.Monad
import Control.Monad.Reader

import Data.Text (Text)

import Language.QuickSilver.Syntax
import Language.QuickSilver.Position
import Language.QuickSilver.TypeCheck.TypedExpr as T
import Language.QuickSilver.Util
import Language.QuickSilver.Generate.Eval
import Language.QuickSilver.Generate.LibQs
import Language.QuickSilver.Generate.Memory.Object
import Language.QuickSilver.Generate.LLVM.Simple
import Language.QuickSilver.Generate.LLVM.Build

fetchCurrentAttr :: Text -> Build Value
fetchCurrentAttr ident = do
  debug $ "fetching current attr " ++ show ident
  curr <- lookupEnv  "Current"
  clas <- currentClass
  obj  <- load curr ""
  debugDump obj
  getAttribute clas ident obj

lookupVarAccess :: UnPosTExpr -> Build Value
lookupVarAccess (Var i _) = lookupEnv i
lookupVarAccess (T.ResultVar _) = lookupEnv "Result"
lookupVarAccess (Access _ i _) = fetchCurrentAttr i
lookupVarAccess e = error ("lookupVarAccess: not Var or Access: " ++ show e)

locOf :: TExpr -> Build Value
locOf te =
  case contents te of
    T.Access trg name _type ->
      do trg' <- eval trg
         accessLoc trg' (T.texpr trg) name
    T.Var n _type -> lookupEnv n
    T.ResultVar _ -> lookupEnv "Result"
    _ -> error $ "locOf: " ++ show te
         

genStmt :: UnPosTStmt -> Build ()
genStmt (CallStmt e)        = eval e >> return ()
genStmt (Block ss)          = mapM_ (genStmt . contents) ss
genStmt (Assign lhs rhs) = do
  debug $ "Assign " ++ show rhs ++ " to " ++ show lhs

  lhsLoc <- locOf lhs
  debug "Assign: lhs"
  debugDump lhsLoc
  
  rhs' <- eval rhs
  debug "Assign: rhs"
  debugDump rhs'

  debug "Assign: storing"
  store rhs' lhsLoc
  debug "Assign: done storing"
  return ()
genStmt (If cond then_ elseIfs elseMb) = do
  debug "genStmt: if"
  condV  <- eval cond

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
        condVal <- eval c
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
  res  <- eval cond
  _ <- condBr res afterB bodyB

  positionAtEnd bodyB
  genStmt (contents body)
  _ <- br condB

  positionAtEnd afterB
  return ()

genStmt (Shutdown e) =
  do e' <- eval e
     currProc <- getCurrProc
     eProc <- getProc e'
     "proc_shutdown" <#> [eProc, currProc]
     return ()

genStmt (Separate args clauses body) =
  do startB  <- getInsertBlock
     func    <- getBasicBlockParent startB
     currProc <- getCurrProc

     getQueuesB <- appendBasicBlock func "get queues block"
     lockB   <- appendBasicBlock func "lock block"
     sepBodyB <- appendBasicBlock func "sep body block"
     retryB  <- appendBasicBlock func "wait cond retry block"
     br getQueuesB

     positionAtEnd getQueuesB
     privQs <- mapM (getProcExpr >=> getQueue) args
     br lockB

     positionAtEnd lockB

     -- Reservation process
     "proc_start_reservation" <#> [currProc]
     let reserveHandler h = "proc_reserve_handler" <#> [currProc, h]
     mapM_ (getProcExpr >=> reserveHandler) args
     "proc_finish_reservation" <#> [currProc]

     -- FIXME: this is here to document how the old reservation worked,
     -- and to make it easier to compare implementations. This should
     -- be removed when it's clear the new implementation is better than
     -- the old.
     -- mapM_ lockSep' privQs

     -- FIXME: raise exception/exit on non-separate failure
     cond <- local (updateQueues (zip args privQs))
                   (evalClauses clauses)
     condBr cond sepBodyB retryB

     positionAtEnd retryB
     unlockQueues privQs
     mapM_ waitOnSeparate args
     br lockB

     positionAtEnd sepBodyB

     local (updateQueues (zip args privQs))
           (genStmt (contents body))
     unlockQueues privQs

genStmt (Passive args body) =
  do currProc <- getCurrProc
     -- this requires that it exist inside a separate block where the
     -- args are already reserved, otherwise the queues won't be in the map
     let sendSync arg =
           do privQ <- getQueueFor arg
              "priv_queue_sync" <#> [privQ, currProc]             
     mapM_ sendSync args
     local (addPassives args) (genStmt (contents body))
     

genStmt (Create _typeMb var fName args) =
  case texpr var of
    Sep _ _ t ->
      do debug "creating separate object"
         varRef <- lookupVarAccess (contents var)
         newSep <- mallocSeparate t
         debugDump newSep

         currProc <- getCurrProc
         newProc <- "proc_new_from_other" <#> [currProc]

         newInst <- lookupMalloc t fName args

         debug "creating separate: storing new proc"
         procLoc <- gepInt newSep [0, 0]
         store newProc procLoc

         debug "creating separate: storing base class"
         instLoc <- gepInt newSep [0, 1]
         voidPtr <- voidPtrType
         newInst' <- bitcast newInst voidPtr "voidPtrInstance"
         store newInst' instLoc
         
         store newSep varRef
         debug ("genStmt: sep create " ++ show var)
         let newCall = CallStmt (attachPos (position var) 
                                 (Call var fName args NoType))

         newQ <- getQueue newProc
         lockSep' newQ
         local (updateQueues [(var, newQ)])
                   (genStmt newCall)
         unlockQueue newQ
         return ()
    varType ->     
      do debug "nonsep create"
         varRef <- lookupVarAccess (contents var)
         debugDump varRef
         debug "create malloc"
         newInst <- lookupMalloc varType fName args
         debugDump newInst
         store newInst varRef
         debug "create call"
         genStmt (CallStmt $
                  attachPos (position var) (Call var fName args NoType)
                 )
         return ()
-- genStmt BuiltIn = lookupBuiltin
genStmt s = error $ "genStmt: no pattern for: " ++ show s

getProcExpr e = eval e >>= getProc

getQueue prc =
    do currProc <- getCurrProc
       "proc_get_queue" <#> [currProc, prc]

lockSep' privQ =
    do currProc <- getCurrProc
       "priv_queue_lock" <#> [privQ, currProc]
       return ()

unlockQueue privQ =
    do debug "unlockQueues"
       currProc <- getCurrProc
       "priv_queue_unlock" <#> [privQ, currProc]

unlockQueues :: [Value] -> Build ()
unlockQueues = mapM_ unlockQueue

waitOnSeparate :: TExpr -> Build ()
waitOnSeparate e =
  do e' <- eval e
     -- FIXME: probably shouldn't re-evaluate the whole expression
     prc <- getProc e'
     currProc <- getCurrProc
     "proc_wait_for_available" <#> [prc, currProc]
     return ()


-- lookupbuiltin :: Build ()
-- lookupBuiltin = do
--   fName <- featureName `fmap` currentFeature
--   cName <- className `fmap` currentClass
--   genBuiltin cName fName

lookupMalloc :: Typ -> Text -> [TExpr] -> Build Value
lookupMalloc t  _fName _args =
  case t of
    ClassType cName _ -> mallocObject cName
    -- FIXME: Add separate wrapper datatype
    Sep _ _ baseType -> mallocObject (classTypeName baseType)
    _ -> error ("lookupMalloc: called on non-class type: " ++ show t)

