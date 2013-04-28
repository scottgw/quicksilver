{-# LANGUAGE ViewPatterns #-}
module Language.QuickSilver.Generate.Eval (eval, loadEval, evalUnPos, true, int) where

import Language.QuickSilver.Syntax (Typ (..))
import Language.QuickSilver.Util
import Language.QuickSilver.Position

import Language.QuickSilver.Generate.Memory.Attribute
import Language.QuickSilver.Generate.Memory.Class
import Language.QuickSilver.Generate.Util
import Language.QuickSilver.Generate.Memory.Type

import Language.QuickSilver.TypeCheck.TypedExpr 

import Language.QuickSilver.Generate.LLVM.Simple
import Language.QuickSilver.Generate.LLVM.Types
import Language.QuickSilver.Generate.LLVM.Util
import Language.QuickSilver.Generate.LLVM.Values

castType :: Typ -> ValueRef -> Build ValueRef
-- castType DoubleType v    = do
--   v' <- load' v
--   dblT <- doubleTypeM
--   siToFP v' dblT "intToDouble" >>= simpStore
castType (ClassType c _) v = do
  t <- lookupClasLType c
  let tp = (pointer0 . pointer0) t
  bitcast v tp ("castTo" ++ c)
castType t _ = error $ "castType: not implemented for " ++ show t

eval :: TExpr -> Build ValueRef
eval (contents -> e) = evalUnPos e

simpStore v = do
  t <- typeOfVal v
  r <- alloca t ""
  store v r
  return r

load' ref = load ref ""
loadEval e = eval e >>= load'

evalUnPos :: UnPosTExpr -> Build ValueRef
evalUnPos (Cast t e) = eval e >>= castType t
evalUnPos (Call trg fName args retVal) = do
  let (ClassType cName _) = texpr trg

  trg'  <- loadEval trg
  debugDump trg'
  args' <- mapM loadEval args
  debug (show trg)
  f <- getNamedFunction (fullNameStr cName fName)
  debug (concat ["eval: call -> " 
                ,fullNameStr cName fName 
                ,",", show f, " with "
                ,show (countParams f)
                ," parameters " 
                ,show (trg:args)])
  debugDump f
  r <- call' f (trg':args') ("call: " ++ fName)
  debug "eval: call -> done"
  setInstructionCallConv r Fast
  
  if retVal /= NoType then simpStore r else return r
evalUnPos (LitInt i)      = int (fromIntegral i) >>= simpStore
evalUnPos (LitDouble d)   = dbl d >>= simpStore
evalUnPos (LitChar c)     = char c >>= simpStore
evalUnPos (LitBool True)  = simpStore =<< true
evalUnPos (LitBool False) = simpStore =<< false
evalUnPos (Access trg attr _) = do
  trgV <- loadEval trg
  let (ClassType cname _) = texprTyp (contents trg)
  clas <- lookupClas cname
  indexM <- attributeOffset clas attr
  case indexM of
    Just index -> gepInt trgV [0,index]
    Nothing -> error $ "evalUnPos: couldn't find index " ++ show (trg, attr)
evalUnPos (Var s _) = lookupEnv s
evalUnPos (CurrentVar _) = lookupEnv "Current"
evalUnPos (ResultVar _) = lookupEnv "Result"
evalUnPos (Box c e) = do
  v    <- loadEval e
  vPtr <- mallocTyp =<< toLLVMType (texpr e)
  _ <- store v vPtr
  vPtrPtr <- simpStore vPtr
  castType c vPtrPtr
evalUnPos (Unbox t e) = do
  ePtr <- loadEval e
  t' <- toLLVMType t
  casted <- bitcast ePtr (pointer0 t') "unboxing cast"
  load casted "unboxing" >>= simpStore
evalUnPos (LitVoid t) = nul `fmap` typeOfM t
evalUnPos (LitString s) = do
  -- we rely that the strings are stored internally as char8*
  rawStr <- getNamedGlobal (s ++ "_global")
  n <- int (length s)
  f <- getNamedFunction (featureAsCreate "STRING_8" "make")
  charType <- int8TypeM
  rawStrPtr <- bitcast rawStr (pointer0 charType) "char8 cast"
  debug "Creating string.. hold on!"
  debugDump f
  debugDump rawStrPtr
  debugDump n
  eiffelString <- call' f [rawStrPtr, n] ("call: string constructor")
  f' <- getNamedFunction (fullNameStr "STRING_8" "make")
  call' f' [eiffelString, rawStrPtr, n] "call: string make"
  simpStore eiffelString
evalUnPos e = error $ "evalUnPos: unhandled case, " ++ show e