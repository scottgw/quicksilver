{-# LANGUAGE OverloadedStrings #-}
module Language.QuickSilver.Generate.Statement where

import Control.Monad

import Data.Text (Text)

import Language.QuickSilver.Syntax
import Language.QuickSilver.Util
import Language.QuickSilver.Position
import Language.QuickSilver.TypeCheck.TypedExpr

import Language.QuickSilver.Generate.Eval
import Language.QuickSilver.Generate.Memory.Class
import Language.QuickSilver.Generate.Memory.Type

import Language.QuickSilver.Generate.LLVM.Simple
import Language.QuickSilver.Generate.LLVM.Util
import Language.QuickSilver.Generate.LLVM.Types

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
lookupVarAccess (Access _ i _) = fetchCurrentAttr i
lookupVarAccess e = error ("lookupVarAccess: not Var or Access: " ++ show e)

genStmt :: UnPosTStmt -> Build ()
genStmt (CallStmt e)        = eval e >> return ()
genStmt (Block ss)          = mapM_ (genStmt . contents) ss
genStmt (Assign ident expr) = do
  debug $ "Assign to: " ++ show ident

  lhs <- eval ident
  rhs <- loadEval expr
  debugDump rhs
  _   <- store rhs lhs
  return ()
genStmt (If b then_ elseIfs elseMb) = do
  bRes   <- loadEval b
  tr     <- true
  cond   <- icmp IntEQ tr bRes "if conditional test"

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
        res     <- icmp IntEQ tr condVal "if conditional test"
        _ <- condBr res elseIfThenB nextIfCase
    
        positionAtEnd elseIfThenB
        genStmt (contents stmt)
        br mergeB
    
        return elseIfB

  elseIfB <- foldM genIfElse elseB (reverse elseIfs)
  
  positionAtEnd thenB
  genStmt (contents then_)
  _ <- br mergeB

  positionAtEnd elseB
  maybe (return ()) (genStmt . contents) elseMb
  _ <- br mergeB

  positionAtEnd startB
  _ <- condBr cond thenB elseIfB

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
  res     <- loadEval cond
  tr      <- true
  condRes <- icmp IntEQ tr res "loop conditional test" 
  _ <- condBr condRes afterB bodyB

  positionAtEnd bodyB
  genStmt (contents body)
  _ <- br condB

  positionAtEnd afterB
  return ()
genStmt (Print e) = do
  v      <- loadEval e
  fmt    <- lookupEnv "intFmtString"
  i8 <- int8TypeM
  let ptr = pointer0 i8
  zero   <- int 0
  fmtRef <- gep fmt [zero, zero]
  fmtCast <- bitcast fmtRef ptr ""
  _ <- call "printf" [fmtCast ,v]
  return ()
genStmt (PrintD e) = do
  v      <- loadEval e
  fmt    <- lookupEnv "dblFmtString"
  zero   <- int 0
  fmtRef <- gep fmt [zero, zero]
  _ <- call "printf" [fmtRef,v]
  return ()
genStmt (Create _typeMb vr fName args) = do
  var <- lookupVarAccess (contents vr)
  newInst <- lookupMalloc (texpr vr) fName args
  _ <- store newInst var

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
lookupMalloc (ClassType cName _) fName args = do
  isCreate <- lookupCreate cName fName
  if isCreate
    then do
      args' <- mapM loadEval args
      callByName (featureAsCreate cName fName) args'
    else unClasRef `fmap` mallocClas cName 
lookupMalloc t _ _ = 
    error ("lookupMalloc: called on non-class type: " ++ show t)

lookupCreate :: Text -> Text -> Build Bool
lookupCreate cName fName = isCreateName fName `fmap` lookupClas cName
