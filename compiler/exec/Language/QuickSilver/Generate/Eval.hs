{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.QuickSilver.Generate.Eval
       ( eval
       , evalClause
       , evalClauses
       -- , loadEval
       , evalUnPos
       , accessLoc
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
import Language.QuickSilver.Generate.LLVM.Build
import Language.QuickSilver.Generate.Memory.Types
import Language.QuickSilver.Generate.Memory.Object

(<#>) :: Text.Text -> [Value] -> Build Value
(<#>) name args =
  do Just ret <- callByName name args
     return ret

evalClause :: Clause TExpr -> Build Value
evalClause (Clause _n e) =
  do debug "evalClause"
     e' <- eval e
     debugDump e'
     return e'

evalClauses :: [Clause TExpr] -> Build Value
evalClauses cs =
  do vals <- mapM evalClause cs
     tr <- true
     foldM (\ acc v -> andd acc v "clause eval fold") tr vals

getCurrProc :: Build Value
getCurrProc = lookupEnv "<CurrentProc>" >>= load'

getProc :: Value -> Build Value
getProc v =
  do procRef <- gepInt v [0, 0]
     load procRef "getProc"

castType :: Typ -> Value -> Build Value
castType Int8Type v =
    do debug "castType: to int8 start"
       i8 <- int8TypeM
       trunc v i8 "castType: to int8"
castType Int64Type v = return v
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
  bitcast v ptrType ("castToAnyRef")
castType t@(Sep _ _ s) v = do
  sepType <- typeOfM t
  bitcast v sepType ("castToSep")
castType t@(ClassType c _) v = do
  tRep <- typeOfM t
  bitcast v tRep ("castTo" ++ Text.unpack c)
castType t _ = error $ "castType: not implemented for " ++ show t

eval :: TExpr -> Build Value
eval (contents -> e) = evalUnPos e

-- simpStore v = do
--   t <- typeOfVal v
--   r <- alloca t ""
--   store v r
--   return r

load' ref = load ref ""
-- loadEval e = eval e >>= load'

genBinOp :: BinOp -> TExpr -> TExpr -> Typ -> Build Value
genBinOp op e1 e2 _resType =
  case lookup op opFuncs of
    Just f ->
      do debug ("genBinOp: " ++ show (op, e1, e2))
         f "genBinOp generated operation"
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
      do e1' <- eval e1
         e2' <- eval e2
         f e1' e2' str

    orElse _str =
      do startB <- getInsertBlock
         func   <- getBasicBlockParent startB
         tr     <- true

         fullEvalB <- appendBasicBlock func "orElse full evaluate"
         afterB    <- appendBasicBlock func "orElse after"

         positionAtEnd startB
         e1'  <- eval e1
         condBr e1' afterB fullEvalB

         positionAtEnd fullEvalB
         e2'  <- eval e2
         br afterB

         positionAtEnd afterB
         phiNode <- join (phi <$> int1TypeM <*> pure "")
         addIncoming phiNode [(tr, startB), (e2', fullEvalB)]
         return phiNode

    andThen _str =
      do startB <- getInsertBlock
         func   <- getBasicBlockParent startB
         fl     <- false

         fullEvalB <- appendBasicBlock func "andThen full evaluate"
         afterB    <- appendBasicBlock func "andThen after"

         positionAtEnd startB
         e1'  <- eval e1
         condBr e1' fullEvalB afterB

         positionAtEnd fullEvalB
         e2'  <- eval e2
         br afterB

         positionAtEnd afterB
         phiNode <- join (phi <$> int1TypeM <*> pure "")
         addIncoming phiNode [(fl, startB), (e2', fullEvalB)]
         return phiNode


genUnOp :: UnOp -> TExpr -> Typ -> Build Value
genUnOp op e _resType =
  case lookup op opFuncs of
    Just f ->
      do debug ("genUnOp: " ++ show (op, e))
         e' <- eval e
         debugDump e'
         f e' "genUnOp generated operation"
    Nothing -> error $ "genUnOp: operation not found: " ++ show op
  where
    opFuncs =
      [ (Neg, if isIntegerType (texpr e)
              then neg
              else fneg)
      , (Not, nott)
      ]

accessLoc :: TExpr -> Text.Text -> Build Value
accessLoc trg attr =
  do debug $ "Access: " ++ show (trg, attr)
     trgV <- eval trg
     let trgType = texpr trg
     baseTrg <- case trgType of
                  Sep _ _ t ->
                      do (_, r) <- splitSepRef trgV
                         inst <- load' r
                         castType t inst
                  _ -> return trgV
     debugDump baseTrg
     let cname = classTypeName trgType
     clas <- lookupClas cname
     case attributeIndex clas attr of
       Just index -> gepInt baseTrg [0, index]
       Nothing ->
         error $ "accessLoc: couldn't find index " ++ show (trg, attr)
 

evalUnPos :: UnPosTExpr -> Build Value
evalUnPos (Cast t e) =
  do debug $ "evalUnPos: cast " ++ show (t, e)
     v <- eval e
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
       Just fn <- getNamedFunction (fullNameStr moduleType name)
       args' <- mapM eval args
       debugDump fn
       mapM_ debugDump (pre ++ args')
       call fn (pre ++ args')

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
     e1' <- eval e1
     debugDump e1'
     
     debug "evalUnPos: 2 eqExpr"
     e2' <- eval e2
     debugDump e2'

     ccmp e1' e2' "equality check"

evalUnPos (Call trg fName args retVal)
    |  isSeparate (texpr trg) = separateCall trg fName args retVal
    |  otherwise              = nonSeparateCall trg fName args retVal

evalUnPos (LitInt i _t)   = int (fromIntegral i)
evalUnPos (LitDouble d)   = dbl d
evalUnPos (LitChar c)     = char c
evalUnPos (LitBool True)  = true
evalUnPos (LitBool False) = false
evalUnPos (Var s _) = lookupEnv s >>= load'
evalUnPos (LitVoid t) = nul `fmap` typeOfM t
evalUnPos (CurrentVar _) = lookupEnv "Current" >>= load'
evalUnPos (ResultVar _) = lookupEnv "Result" >>= load'

evalUnPos (UnOpExpr op e resType) = genUnOp op e resType
evalUnPos (BinOpExpr op e1 e2 resType) = genBinOp op e1 e2 resType

evalUnPos (Access trg attr typ) = do
  attrLoc <- accessLoc trg attr
  let trgType = texpr trg
  debug "access: have location"
  if isSeparate trgType
    then
      do startB <- getInsertBlock
         func   <- getBasicBlockParent startB
         afterAccessB <- appendBasicBlock func "afterAccess"
         directAccessB <- appendBasicBlock func "directAccess"
         sepAccessB <- appendBasicBlock func "sepAccess"

         privQ <- getQueueFor trg
         
         lastWasFunc <- "priv_queue_last_was_func" <#> [privQ]

         condBr lastWasFunc directAccessB sepAccessB

         positionAtEnd directAccessB
         directVal <- load' attrLoc
         br afterAccessB

         positionAtEnd sepAccessB
         closT <- closType typ
         voidPtr <- voidPtrType
         closResLoc <- closLocFor typ
         attrVoidPtr <- bitcast attrLoc voidPtr ""
         closResVoidPtr <- bitcast closResLoc voidPtr ""
         currProc <- getCurrProc
         mapM_ debugDump [privQ, closT, attrVoidPtr, closResLoc, currProc]
         "priv_queue_access" <#> 
            [privQ, closT, attrVoidPtr, closResVoidPtr, currProc]
         debug "sep access done"
         sepCallVal <- load' closResLoc
         br afterAccessB

         -- Really should use a phi node here.
         positionAtEnd afterAccessB
         phiNode <- join (phi <$> typeOfM typ <*> pure "")
         addIncoming phiNode [ (directVal, directAccessB)
                         , (sepCallVal, sepAccessB)
                         ]
         return phiNode
    else debugDump attrLoc >> load' attrLoc

evalUnPos (InheritProc _ e) = eval e

-- evalUnPos (Box c e) = do
--   v    <- eval e
--   vPtr <- mallocTyp =<< typeOfM (texpr e)
--   _ <- store v vPtr
--   vPtrPtr <- simpStore vPtr
--   castType c vPtrPtr

-- evalUnPos (Unbox t e) = do
--   ePtr <- eval e
--   t' <- typeOfM t
--   casted <- bitcast ePtr (pointer0 t') "unboxing cast"
--   load casted "unboxing"

evalUnPos (LitString s) = do
  -- we rely that the strings are stored internally as char8*
  Just rawStr <- getNamedGlobal (s `Text.append` "_global")
  n <- int (Text.length s)
  Just f <- getNamedFunction (fullNameStr "String" "make_with_pointer")
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
  call f [currProc, strPtr, n, rawStrPtr]
  debug "Creating string done"
  return strPtr
evalUnPos e = error $ "evalUnPos: unhandled case, " ++ show e


nonSeparateCall trg fName args retType =
    do let cName = classTypeName (texpr trg)

       currProc <- getCurrProc
       trg'  <- eval trg
       debugDump trg'
       args' <- mapM eval args
       debug (show trg)
       Just f <- getNamedFunction (fullNameStr cName fName)
       debug (concat ["eval: call -> " 
                     ,Text.unpack $ fullNameStr cName fName 
                     ,",", show f, " with "
                     ,show (countParams f)
                     ," parameters " 
                     ,show (trg:args)])
       debugDump f

       call f (currProc:trg':args')

separateCall trg fName args retType =
  do let cName = classTypeName (texpr trg)
     debug "generating separate call"

     -- positionAtEnd sepCallStartB
     trg'  <- eval trg
     debugDump trg'
     args' <- mapM eval args
     debug (show trg)
     Just f <- getNamedFunction (fullNameStr cName fName)
     debug (concat ["sepCall: call -> "
                   ,Text.unpack $ fullNameStr cName fName 
                   ,",", show f, " with "
                   ,show (countParams f)
                   ," parameters " 
                   ,show (trg:args)])
     debugDump f

     (trgProcRef, nonSepTrgRef) <- splitSepRef trg'
     trgProc <- load' trgProcRef
     nonSepTrg <- load' nonSepTrgRef
     privQ <- getQueueFor trg

     if retType == NoType
       then
         do closure <- prepareClosure retType trg f args trgProc nonSepTrg args'
            debug "sepCall: calling underlying function"
            currProc <- getCurrProc
            "priv_queue_routine" <#> [privQ, closure, currProc]
       else
         do startB <- getInsertBlock
            func   <- getBasicBlockParent startB

            directFuncB <- appendBasicBlock func "directFuncCall"
            sepFuncCallB <- appendBasicBlock func "sepFuncCall"
            afterSepCallB <- appendBasicBlock func "afterSepCall"

            lastWasFunc <- "priv_queue_last_was_func" <#> [privQ]
            condBr lastWasFunc directFuncB sepFuncCallB

            positionAtEnd directFuncB
            debug "building direct call"
            let Sep _ _ baseType = texpr trg
            baseTypeL <- typeOfM baseType
            castedNonSepTrg <- bitcast nonSepTrg baseTypeL ""
            directVal <- call f (trgProc : castedNonSepTrg : args')
            br afterSepCallB

            positionAtEnd sepFuncCallB
            closure <- prepareClosure retType trg f args trgProc nonSepTrg args'
            debug "sepCall: calling underlying function"
            currProc <- getCurrProc
            voidPtr <- voidPtrType

            -- Dig out the appropriate storage type and cast to a void
            -- pointer priv_queue_function can take it.
            resLoc <- closLocFor retType
            resLocCast <- bitcast resLoc voidPtr "closure_result_cast"
            "priv_queue_function" <#> [privQ, closure, resLocCast, currProc]
            sepCallVal <- load' resLoc
            br afterSepCallB

            positionAtEnd afterSepCallB
            phiNode <- join (phi <$> typeOfM retType <*> pure "")
            addIncoming phiNode [ (directVal, directFuncB)
                                , (sepCallVal, sepFuncCallB)
                                ]
            if not (isBasic retType || isSeparate retType)
              then wrapSepResult trgProc retType phiNode
              else return phiNode

prepareClosure retType trg f args trgProc nonSepTrg args' =
  do let Sep _ _ nonSepTrgType = texpr trg
         allTypes = ProcessorType : nonSepTrgType : map texpr args 
         allEvald = trgProc : nonSepTrg : args'
         n = length allTypes

     funcPtr <- join (bitcast f <$> voidPtrType <*> pure "cast func to ptr")
     closRetType <- closType retType
     argCount <- int n
     argArrayPtrRef <- lookupEnv "<args>"
     argTypeArrayRef <- lookupEnv "<argTypes>"
  
     closure <- "closure_new" <#> [ funcPtr
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
                     then join (intToPtr val <$> voidPtrType <*> pure "")
                     else join (bitcast val <$> voidPtrType <*> pure "")
              argLoc <- gepInt argArrayPtr [idx]
              argRef <- load' argLoc
              store ptr argRef

     zipWithM storeType allTypes [0 ..]
     zipWithM storeArg (zip allTypes allEvald) [0 ..]
     return closure

splitSepRef ref = (,) <$> gepInt ref [0, 0] <*> gepInt ref [0, 1]

wrapSepResult trgProc retType result =
  do sepInst <- mallocSeparate retType
     (procRef, objRef) <- splitSepRef sepInst
     store trgProc procRef
     store result objRef
     return sepInst

getQueueFor :: TExpr -> Build Value
getQueueFor e =
    case contents e of
      InheritProc inh _ -> getQueueFor inh
      _ -> lookupQueueFor e

closLocFor :: Typ -> Build Value
closLocFor t =
    case t of
      BoolType -> lookupEnv "<closResult1>"
      Int64Type -> lookupEnv "<closResult64>"
      ClassType _ _ -> lookupEnv "<closResult64>"
      NoType -> lookupEnv "<closResult64>"
      e -> error ("closLocFor: " ++ show e)

closType :: Typ -> Build Value
closType t =
    case t of
      BoolType -> "closure_uint1_type" <#> []
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
