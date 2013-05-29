{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.QuickSilver.Generate.Eval
       ( eval
       , evalClause
       , evalClauses
       , loadEval
       , evalUnPos
       , getCurrProc
       , getProc
       , true
       , int
       , (<#>)
       ) where

import Control.Applicative
import Control.Monad

-- import Data.Text (Text)
import qualified Data.Text as Text

import Language.QuickSilver.Syntax
    (Typ (..), BinOp(..), UnOp(..), ROp(..)
    , AbsRoutine(..), EmptyBody(..), Clause(..))
import Language.QuickSilver.Util
import Language.QuickSilver.Position
import Language.QuickSilver.TypeCheck.TypedExpr as T
import Language.QuickSilver.Generate.LibQs
import Language.QuickSilver.Generate.LLVM.Simple
import Language.QuickSilver.Generate.LLVM.Types
import Language.QuickSilver.Generate.LLVM.Util
import Language.QuickSilver.Generate.LLVM.Values
import Language.QuickSilver.Generate.Memory.Types
import Language.QuickSilver.Generate.Memory.Object
import Language.QuickSilver.Generate.Util


(<#>) = callByName


evalClause :: Clause TExpr -> Build ValueRef
evalClause (Clause _n e) = loadEval e

evalClauses :: [Clause TExpr] -> Build ValueRef
evalClauses cs =
  do vals <- mapM evalClause cs
     tr <- true
     foldM (\ acc v -> andd acc v "clause eval fold") tr vals

getCurrProc :: Build ValueRef
getCurrProc = lookupEnv "<CurrentProc>" >>= load'

getProc :: ValueRef -> Build ValueRef
getProc v =
  do procRef <- gepInt v [0, 0]
     load procRef "getProc"

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
castType (AnyRefType _) v = do
  ptrType <- voidPtrType
  bitcast v ptrType ("castToAnyRef") >>= simpStore
castType t@(Sep _ _ s) v = do
  sepType <- typeOfM t
  bitcast v sepType ("castToSep") >>= simpStore
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
      , (Mul, strictApply mul)
      , (Rem, strictApply srem)
      , (Quot, strictApply sdiv)
      , (Div, if isIntegerType (texpr e1)
              then strictApply sdiv
              else strictApply fdiv)
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
       Just rout <- findAbsRoutine <$> lookupClas moduleType <*> pure name
       pre <- case routineImpl rout of
                -- externals do not have an implicit processor parameter
                -- in modules.
                EmptyExternal _ _ -> return []
                _ -> do debug "evalUnPos: static call forwards current proc"
                        procRef <- lookupEnv "<CurrentProc>"
                        proc <- load procRef "loading proc for static call"
                        return [proc]
       fn <- getNamedFunction (fullNameStr moduleType name)
       args' <- mapM loadEval args
       debugDump fn
       mapM_ debugDump (pre ++ args')
       r <- call' fn (pre ++ args') ("static call: " ++ Text.unpack name)
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

evalUnPos (Call trg fName args retVal)
    |  isSeparate (texpr trg) = separateCall trg fName args retVal
    |  otherwise              = nonSeparateCall trg fName args retVal

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
  let cname = classTypeName (texprTyp (contents trg))
  clas <- lookupClas cname
  case attributeIndex clas attr of
    -- Add 1 to skip the processor in every object.
    Just index -> gepInt trgV [0, index] 
    Nothing -> error $ "evalUnPos: couldn't find index " ++ show (trg, attr)

evalUnPos (InheritProc _ e) = eval e

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
  currProc <- getCurrProc
  charPtrType <- pointer0 <$> int8TypeM
  rawStrPtr <- bitcast rawStr charPtrType "char8 cast"
  debugDump f
  debugDump rawStrPtr
  debugDump n
  debug "Creating string.. hold on!"
  strPtr <- mallocObject "String"
  debugDump strPtr
 --  str <- load strPtr "loading created string"
  -- debugDump str
  call' f [currProc, strPtr, n, rawStrPtr] ("call: string constructor")
  debug "Creating string done"
  simpStore strPtr
evalUnPos e = error $ "evalUnPos: unhandled case, " ++ show e


nonSeparateCall trg fName args retVal =
    do let cName = classTypeName (texpr trg)

       currProc <- getCurrProc
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

       r <- call' f (currProc:trg':args') ("nonsep call: " ++ Text.unpack fName)
       debug "eval: nonsep call -> done"
       -- setInstructionCallConv r Fast

       if retVal /= NoType then simpStore r else return r

separateCall trg fName args retVal =
    do let cName = classTypeName (texpr trg)
       debug "generating separate call"
       trg'  <- loadEval trg
       debugDump trg'
       args' <- mapM loadEval args
       debug (show trg)
       f <- getNamedFunction (fullNameStr cName fName)
       debug (concat ["sepCall: call -> " 
                     ,Text.unpack $ fullNameStr cName fName 
                     ,",", show f, " with "
                     ,show (countParams f)
                     ," parameters " 
                     ,show (trg:args)])
       debugDump f

       trgProcRef <- gepInt trg' [0, 0]
       trgProc <- load' trgProcRef
       nonSepTrgRef <- gepInt trg' [0, 1]
       nonSepTrg <- load' nonSepTrgRef

       let Sep _ _ nonSepTrgType = texpr trg
           allTypes = ProcessorType : nonSepTrgType : map texpr args 
           allEvald = trgProc : nonSepTrg : args'
           n = length allTypes

       funcPtr <- join (bitcast f <$> voidPtrType <*> pure "cast func to ptr")
       closRetType <- closType retVal
       argCount <- int n
       argArrayPtrRef <- lookupEnv "<args>"
       argTypeArrayRef <- lookupEnv "<argTypes>"

       closure <-
           "closure_new" <#> [ funcPtr
                             , closRetType
                             , argCount
                             , argArrayPtrRef
                             , argTypeArrayRef
                             ]

       debug "sepCall: filling type and arg arrays"

       argTypeArray <- load' argTypeArrayRef
       argArrayPtr <- load' argArrayPtrRef

       let storeType t idx =
               do debug $ "storeType: " ++ show (t, idx)
                  closT <- closType t
                  debugDump argTypeArray
                  argRef <- gepInt argTypeArray [idx]
                  store closT argRef

           storeArg (t, val) idx =
               do debug $ "storeArg: " ++ show (t, val, idx)
                  ptr <- if isBasic t
                         then join (intToPtr val <$> voidPtrType <*> pure "ptrtoint")
                         else join (bitcast val <$> voidPtrType <*> pure "arg store bitcast")
                  argLoc <- gepInt argArrayPtr [idx]
                  argRef <- load' argLoc
                  store ptr argRef
                  -- argArray <- load' argArrayPtr
       
                  -- argRef <- gepInt argArray [idx]
                  -- store ptr argRef

       zipWithM storeType allTypes [0 ..]
       zipWithM storeArg (zip allTypes allEvald) [0 ..]

       debug "sepCall: calling underlying function"

       privQ <- getQueueFor trg

       currProc <- getCurrProc

       if retVal == NoType
         then "priv_queue_routine" <#> [privQ, closure, currProc]
         else
           do resType <- typeOfM retVal
              resLoc <- lookupEnv "<closResult>"
              voidPtr <- voidPtrType
              resLocCast <- bitcast resLoc voidPtr "closure_result_cast"
              "priv_queue_function" <#> [privQ, closure, resLocCast, currProc]
              if isBasic retVal || isSeparate retVal
                then return resLoc
                     -- Basic and results that are declared separate do not
                     -- need to be re-wrapped with the processor of
                     -- their target.
                else
                  do sepInst <- mallocSeparate retVal
                     procRef <- gepInt sepInst [0, 0]
                     objRef <- gepInt sepInst [0, 1]
                     res <- load' resLoc
                     store trgProc procRef
                     store res objRef

                     sepRef <- join (alloca <$> typeOfVal sepInst <*> pure "sepInstRef")
                     store sepInst sepRef

getQueueFor :: TExpr -> Build ValueRef
getQueueFor e =
    case contents e of
      InheritProc inh _ -> getQueueFor inh
      _ -> lookupQueueFor e

closType :: Typ -> Build ValueRef
closType t =
    case t of
      AnyIntType -> "closure_sint_type" <#> []
      Int8Type -> "closure_sint8_type" <#> []
      Int16Type -> "closure_sint16_type" <#> []
      Int32Type -> "closure_sint32_type" <#> []
      Int64Type -> "closure_sint_type" <#> []
      ClassType _ _ -> "closure_pointer_type" <#> []
      NoType -> "closure_void_type" <#> []
      ProcessorType -> "closure_pointer_type" <#> []
      Sep _ _ _ -> "closure_pointer_type" <#> []
      _ -> error $ "closType: " ++ show t
