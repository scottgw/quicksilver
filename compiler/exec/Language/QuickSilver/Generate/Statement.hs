{-# LANGUAGE OverloadedStrings #-}
module Language.QuickSilver.Generate.Statement (genStmt) where

import Control.Monad

import Data.Text (Text)

import Language.QuickSilver.Syntax
import Language.QuickSilver.Util
import Language.QuickSilver.Position
import Language.QuickSilver.TypeCheck.TypedExpr as T
import Language.QuickSilver.Generate.Eval
import Language.QuickSilver.Generate.Memory.Class
import Language.QuickSilver.Generate.LLVM.Simple
import Language.QuickSilver.Generate.LLVM.Util

fetchCurrentAttr :: Text -> Build ValueRef
fetchCurrentAttr ident = do
  curr <- lookupEnv  "Current"
  clas <- currentClass
  obj  <- load curr "lookupVarOrAttr load"
  getAttribute clas ident obj

lookupVarOrAttr :: Text -> Build ValueRef
lookupVarOrAttr ident =
  lookupEnvM ident >>= maybe (fetchCurrentAttr ident) return

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
  do -- FIXME: add preparation for the args before generating the body
     genStmt (contents body)
  
genStmt (Create _typeMb vr fName args) = do
  var <- lookupVarAccess (contents vr)
  newInst <- lookupMalloc (texpr vr) fName args
  loc <- gepInt newInst [0, 0]
  procRef <- lookupEnv "<CurrentProc>"
  proc <- load procRef "Loading <CurrentProc>"
  store proc loc
  store newInst var

  genStmt (CallStmt $ attachPos (position vr) (Call vr fName args NoType))
  return ()
-- genStmt BuiltIn = lookupBuiltin
genStmt s = error $ "genStmt: no pattern for: " ++ show s

-- lookupBuiltin :: Build ()
-- lookupBuiltin = do
--   fName <- featureName `fmap` currentFeature
--   cName <- className `fmap` currentClass
--   genBuiltin cName fName


lookupMalloc :: Typ -> Text -> [TExpr] -> Build ValueRef
lookupMalloc t  fName _args =
  case t of
    ClassType cName _ -> processClass cName
    Sep _ _ cName -> processClass cName
    _ -> error ("lookupMalloc: called on non-class type: " ++ show t)
 where
   processClass cName =
      do isCreate <- lookupCreate cName fName
         if isCreate
           then do
             -- args' <- mapM loadEval args
             unClasRef `fmap` mallocClas cName 
             -- callByName (featureAsCreate cName fName) args'
           else unClasRef `fmap` mallocClas cName 

lookupCreate :: Text -> Text -> Build Bool
lookupCreate cName fName = isCreateName fName `fmap` lookupClas cName
