{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.QuickSilver.Generate.Eval (eval, loadEval, evalUnPos, true, int) where

import Control.Applicative
import Control.Monad

-- import Data.Text (Text)
import qualified Data.Text as Text

import Language.QuickSilver.Syntax (Typ (..), BinOp(..), UnOp(..), ROp(..))
import Language.QuickSilver.Util
import Language.QuickSilver.Position
import Language.QuickSilver.TypeCheck.TypedExpr as T
import Language.QuickSilver.Generate.LLVM.Simple
import Language.QuickSilver.Generate.LLVM.Types
import Language.QuickSilver.Generate.LLVM.Util
import Language.QuickSilver.Generate.LLVM.Values
import Language.QuickSilver.Generate.Memory.Attribute
import Language.QuickSilver.Generate.Memory.Class
import Language.QuickSilver.Generate.Util

castType :: Typ -> ValueRef -> Build ValueRef
castType Int8Type v =
    do debug "castType: to int8 start"
       i8 <- int8TypeM
       trunc v i8 "castType: to int8" >>= simpStore
castType Int64Type v = simpStore v
    -- do v' <- load' v
    --    debugDump v'
    --    i64 <- int64TypeM
    --    sext v' i64 "castType: to int64" >>= simpStore
-- castType DoubleType v    = do
--   v' <- load' v
--   dblT <- doubleTypeM
--   siToFP v' dblT "intToDouble" >>= simpStore
castType t@(ClassType c _) v = do
  tRep <- typeOfM t
  bitcast v tRep ("castTo" ++ Text.unpack c) >>= simpStore
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
  case lookup op opFuncs of
    Just f ->
      do debug ("genBinOp: " ++ show (op, e1, e2))
         v <- f "genBinOp generated operation"
         simpStore v
    Nothing -> error $ "genBinOp: operation not found: " ++ show op
  where
    opFuncs =
      [ (Add, strictApply add)
      , (Sub, strictApply sub)
      , (Or, strictApply orr)
      , (And, strictApply andd)
      , (OrElse, orElse)
      , (AndThen, andThen)
      , (RelOp Gt NoType, if isIntegerType (texpr e1)
                          then strictApply (icmp IntSGT)
                          else strictApply (fcmp FPOGT))
      , (RelOp Gte NoType, if isIntegerType (texpr e1)
                           then strictApply (icmp IntSGE)
                           else strictApply (fcmp FPOGE))
      , (RelOp Lt NoType, if isIntegerType (texpr e1)
                          then strictApply (icmp IntSLT)
                          else strictApply (fcmp FPOLT))
      , (RelOp Lte NoType, if isIntegerType (texpr e1)
                           then strictApply (icmp IntSLE)
                           else strictApply (fcmp FPOLE))        
      ]

    strictApply f str =
      do e1' <- loadEval e1
         e2' <- loadEval e2
         f e1' e2' str

    orElse _str =
      do startB <- getInsertBlock
         func   <- getBasicBlockParent startB
         res    <- join (alloca <$> int1TypeM <*> pure "orElse result")
         tr     <- true

         shortcutB <- appendBasicBlock func "orElse shortcut"
         fullEvalB <- appendBasicBlock func "orElse full evaluate"
         afterB    <- appendBasicBlock func "orElse after"

         positionAtEnd startB
         e1'  <- loadEval e1
         condBr e1' shortcutB fullEvalB

         positionAtEnd fullEvalB
         e2'  <- loadEval e2
         store e2' res
         br afterB

         positionAtEnd shortcutB
         store tr res
         br afterB

         positionAtEnd afterB
         debug "orElse res"
         debugDump res
         load res "orElse result"

    andThen _str =
      do startB <- getInsertBlock
         func   <- getBasicBlockParent startB
         res    <- join (alloca <$> int1TypeM <*> pure "andThen result")
         fl     <- false

         shortcutB <- appendBasicBlock func "andThen shortcut"
         fullEvalB <- appendBasicBlock func "andThen full evaluate"
         afterB    <- appendBasicBlock func "andThen after"

         positionAtEnd startB
         e1'  <- loadEval e1
         condBr e1' fullEvalB shortcutB

         positionAtEnd fullEvalB
         e2'  <- loadEval e2
         store e2' res
         br afterB

         positionAtEnd shortcutB
         store fl res
         br afterB

         positionAtEnd afterB
         debug "andThen res"
         debugDump res
         load res "andThen result"


genUnOp :: UnOp -> TExpr -> Typ -> Build ValueRef
genUnOp op e _resType =
  case lookup op opFuncs of
    Just f ->
      do debug ("genUnOp: " ++ show (op, e))
         e' <- loadEval e
         debugDump e'
         v <- f e' "genUnOp generated operation"
         debugDump v
         simpStore v
    Nothing -> error $ "genUnOp: operation not found: " ++ show op
  where
    opFuncs =
      [ (Neg, if isIntegerType (texpr e)
              then neg
              else fneg)
      , (Not, nott)
      ]


evalUnPos :: UnPosTExpr -> Build ValueRef
evalUnPos (Cast t e) =
  do debug $ "evalUnPos: cast " ++ show (t, e)
     v <- loadEval e
     castType t v
evalUnPos (StaticCall (ClassType moduleType _) name args retVal) =
    do debug "evalUnPos: static call"
       -- modul <- lookupClas (classNameType moduleType)
       fn <- getNamedFunction (fullNameStr moduleType name)
       args' <- mapM loadEval args
       debugDump fn
       mapM_ debugDump args'
       r <- call' fn args' ("static call: " ++ Text.unpack name)
       -- setInstructionCallConv r Fast
       if retVal /= NoType then simpStore r else return r

evalUnPos (EqExpr op e1 e2) =
  do let ccmp | isIntegerType (texpr e1) || texpr e1 == CharType=
                   icmp $ case op of
                     T.Neq -> IntNE
                     T.Eq -> IntEQ
              | otherwise = \ v1 v2 str ->
                   do i64 <- int64TypeM
                      i1 <- ptrToInt v1 i64 "1 ptrToInt"
                      i2 <- ptrToInt v2 i64 "2 ptrToInt"
                      let llvmOp = case op of
                            T.Neq -> IntNE
                            T.Eq -> IntEQ
                      debug "ptr compare"
                      debugDump i1
                      debugDump i2
                      icmp llvmOp i1 i2 str
     debug $ "evalUnPos: eqExpr " ++ show (op, e1, e2)
     
     debug "evalUnPos: 1 eqExpr"
     e1' <- loadEval e1
     debugDump e1'
     
     debug "evalUnPos: 2 eqExpr"
     e2' <- loadEval e2
     debugDump e2'

     res <- ccmp e1' e2' "equality check"
     debug "evalUnPos: eqExpr done"
     simpStore res

evalUnPos (Call trg fName args retVal) = do
  let cName = case  texpr trg of
        ClassType cName _ -> cName
        Sep _ _ cName -> cName

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
  -- setInstructionCallConv r Fast
  
  if retVal /= NoType then simpStore r else return r

evalUnPos (LitInt i _t)   = int (fromIntegral i) >>= simpStore
evalUnPos (LitDouble d)   = dbl d >>= simpStore
evalUnPos (LitChar c)     = char c >>= simpStore
evalUnPos (LitBool True)  = simpStore =<< true
evalUnPos (LitBool False) = simpStore =<< false
evalUnPos (Var s _) = lookupEnv s
evalUnPos (LitVoid t) = nul `fmap` typeOfM t >>= simpStore
evalUnPos (CurrentVar _) = lookupEnv "Current"
evalUnPos (ResultVar _) = lookupEnv "Result"

evalUnPos (UnOpExpr op e resType) = genUnOp op e resType
evalUnPos (BinOpExpr op e1 e2 resType) = genBinOp op e1 e2 resType

evalUnPos (Access trg attr typ) = do
  debug $ "Access: " ++ show (trg, attr, typ)
  trgV <- loadEval trg
  debugDump trgV
  let (ClassType cname _) = texprTyp (contents trg)
  clas <- lookupClas cname
  case attributeIndex clas attr of
    Just index -> gepInt trgV [0, index]
    Nothing -> error $ "evalUnPos: couldn't find index " ++ show (trg, attr)

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
  debugDump strPtr
 --  str <- load strPtr "loading created string"
  -- debugDump str
  call' f [strPtr, n, rawStrPtr] ("call: string constructor")
  debug "Creating string done"
  simpStore strPtr
evalUnPos e = error $ "evalUnPos: unhandled case, " ++ show e
