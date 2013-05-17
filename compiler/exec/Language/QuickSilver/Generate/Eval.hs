{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.QuickSilver.Generate.Eval (eval, loadEval, evalUnPos, true, int) where

import Control.Applicative

-- import Data.Text (Text)
import qualified Data.Text as Text

import Language.QuickSilver.Syntax (Typ (..), BinOp(..), ROp(..))
import Language.QuickSilver.Util
import Language.QuickSilver.Position
import Language.QuickSilver.TypeCheck.TypedExpr 
import Language.QuickSilver.Generate.LLVM.Simple
import Language.QuickSilver.Generate.LLVM.Types
import Language.QuickSilver.Generate.LLVM.Util
import Language.QuickSilver.Generate.LLVM.Values
import Language.QuickSilver.Generate.Memory.Attribute
import Language.QuickSilver.Generate.Memory.Class
import Language.QuickSilver.Generate.Util

castType :: Typ -> ValueRef -> Build ValueRef
-- castType DoubleType v    = do
--   v' <- load' v
--   dblT <- doubleTypeM
--   siToFP v' dblT "intToDouble" >>= simpStore
castType (ClassType c _) v = do
  t <- lookupClasLType c
  let tp = (pointer0 . pointer0) t
  bitcast v tp ("castTo" ++ Text.unpack c)
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

genBinOp :: BinOp -> TExpr -> TExpr -> Typ -> Build ValueRef
genBinOp op e1 e2 _resType =
  case lookup (op, texpr e1, texpr e2) opFuncs of
    Just f ->
      do e1' <- loadEval e1
         e2' <- loadEval e2
         v <- f e1' e2' "genBinOp generated operation"
         simpStore v
    Nothing -> error "genBinOp: operation not found"
  where
    opFuncs =
      [ ((Add, IntType, IntType), add)
      , ((RelOp Gt NoType, IntType, IntType), icmp IntSGT)
      , ((RelOp Gte NoType, IntType, IntType), icmp IntSGE)
      ]
evalUnPos :: UnPosTExpr -> Build ValueRef
evalUnPos (Cast t e) = eval e >>= castType t
evalUnPos (StaticCall _moduleType name args retVal) =
    do debug "evalUnPos: static call"
       -- modul <- lookupClas (classNameType moduleType)
       fn <- getNamedFunction name
       args' <- mapM loadEval args
       debugDump fn
       mapM debugDump args'
       r <- call' fn args' ("static call: " ++ Text.unpack name)
       setInstructionCallConv r Fast
       if retVal /= NoType then simpStore r else return r
evalUnPos (Call trg fName args retVal) = do
  let (ClassType cName _) = texpr trg

  trg'  <- loadEval trg
  debugDump trg'
  args' <- mapM loadEval args
  debug (show trg)
  f <- getNamedFunction (fullNameStr cName fName)
  debug (concat ["eval: call -> " 
                ,Text.unpack $ fullNameStr cName fName 
                ,",", show f, " with "
                ,show (countParams f)
                ," parameters " 
                ,show (trg:args)])
  debugDump f
  r <- call' f (trg':args') ("call: " ++ Text.unpack fName)
  debug "eval: call -> done"
  setInstructionCallConv r Fast
  
  if retVal /= NoType then simpStore r else return r
evalUnPos (LitInt i)      = int (fromIntegral i) >>= simpStore
evalUnPos (LitDouble d)   = dbl d >>= simpStore
evalUnPos (LitChar c)     = char c >>= simpStore
evalUnPos (LitBool True)  = simpStore =<< true
evalUnPos (LitBool False) = simpStore =<< false
evalUnPos (BinOpExpr op t1 t2 resType) = genBinOp op t1 t2 resType
evalUnPos (Access trg attr _) = do
  trgV <- loadEval trg
  let (ClassType cname _) = texprTyp (contents trg)
  clas <- lookupClas cname
  case attributeIndex clas attr of
    Just index -> gepInt trgV [0,index]
    Nothing -> error $ "evalUnPos: couldn't find index " ++ show (trg, attr)
evalUnPos (Var s _) = lookupEnv s
evalUnPos (CurrentVar _) = lookupEnv "Current"
evalUnPos (ResultVar _) = lookupEnv "Result"
evalUnPos (Box c e) = do
  v    <- loadEval e
  vPtr <- mallocTyp =<< typeOfM (texpr e)
  _ <- store v vPtr
  vPtrPtr <- simpStore vPtr
  castType c vPtrPtr
evalUnPos (Unbox t e) = do
  ePtr <- loadEval e
  t' <- typeOfM t
  casted <- bitcast ePtr (pointer0 t') "unboxing cast"
  load casted "unboxing" >>= simpStore
evalUnPos (LitVoid t) = nul `fmap` typeOfM t
evalUnPos (LitString s) = do
  -- we rely that the strings are stored internally as char8*
  rawStr <- getNamedGlobal (s `Text.append` "_global")
  n <- int (Text.length s)
  f <- getNamedFunction (fullNameStr "String" "make_with_pointer")
  charPtrType <- pointer0 <$> int8TypeM
  rawStrPtr <- bitcast rawStr charPtrType "char8 cast"
  debugDump f
  debugDump rawStrPtr
  debugDump n
  debug "Creating string.. hold on!"
  strPtr <- unClasRef <$> mallocClas "String"
 --  str <- load strPtr "loading created string"
  -- debugDump str
  call' f [strPtr, n, rawStrPtr] ("call: string constructor")
  simpStore strPtr
evalUnPos e = error $ "evalUnPos: unhandled case, " ++ show e
