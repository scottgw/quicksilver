{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.QuickSilver.TypeCheck.Expr 
       (typeOfExpr, typeOfExprIs, clause) where

import           Control.Applicative
import           Control.Lens hiding (op)
import           Control.Monad.Error
import           Control.Monad.Reader


import           Data.Generics
import qualified Data.Text as Text

import           Language.QuickSilver.Syntax
import           Language.QuickSilver.Position
import           Language.QuickSilver.Util
import qualified Language.QuickSilver.TypeCheck.TypedExpr as T
import           Language.QuickSilver.TypeCheck.TypedExpr (TExpr)
import           Language.QuickSilver.TypeCheck.BasicTypes
import           Language.QuickSilver.TypeCheck.Context
import           Language.QuickSilver.TypeCheck.Generic

checkBinOp :: BinOp -> TExpr -> TExpr -> TypingBody body TExpr
checkBinOp op e1 e2 
    | isNumOp = numOp
    | isCompOp = compOp
    | isEqOp   = equalOp
    | isBoolOp = tagPos (T.BinOpExpr op e1 e2 BoolType)
    | otherwise = err
    where
      err = throwError $ concat ["checkBinOp: no infix operation for: "
                                , show (op, t1, t2)
                                ]

      isEqOp   = op `elem` (map (flip RelOp NoType) [Eq, Neq])
      isNumOp  = op `elem` [Add, Sub, Mul, Div, Rem, Quot]
      isBoolOp  = op `elem` [And, Or, OrElse, AndThen]
      isCompOp = op `elem` (map (flip RelOp NoType) [Gt, Lt, Gte, Lte])

      t1 = T.texpr e1
      t2 = T.texpr e2

      equalOp
        | t1 == t2 = tagPos (T.EqExpr (T.eqOp op) e1 e2)
        | t1 == AnyIntType && isIntegerType t2 =
            do e1' <- tagPos (T.Cast t2 e1)
               tagPos (T.EqExpr (T.eqOp op) e1' e2)
        | t2 == AnyIntType && isIntegerType t1 =
            do e2' <- tagPos (T.Cast t1 e2)
               tagPos (T.EqExpr (T.eqOp op) e1 e2')
        | t1 == VoidType && not (isBasic t2) =
            do e1' <- tagPos (T.Cast t2 e1)
               tagPos (T.EqExpr (T.eqOp op) e1' e2)
        | t2 == VoidType && not (isBasic t1) =
            do e2' <- tagPos (T.Cast t1 e2)
               tagPos (T.EqExpr (T.eqOp op) e1 e2')
        | otherwise = err

      numOp
        | t1 == t2 = tagPos (T.BinOpExpr op e1 e2 t1)
        | t1 == AnyIntType && isIntegerType t2 =
            do e1' <- tagPos (T.Cast t2 e1)
               tagPos (T.BinOpExpr op e1' e2 t2)
        | t2 == AnyIntType && isIntegerType t1 =
            do e2' <- tagPos (T.Cast t1 e2)
               tagPos (T.BinOpExpr op e1 e2' t1)
        | otherwise = err

      compOp
        | t1 == t2 = tagPos (T.BinOpExpr op e1 e2 BoolType)
        | t1 == AnyIntType && isIntegerType t2 =
            do e1' <- tagPos (T.Cast t2 e1)
               tagPos (T.BinOpExpr op e1' e2 BoolType)
        | t2 == AnyIntType && isIntegerType t1 =
            do e2' <- tagPos (T.Cast t1 e2)
               tagPos (T.BinOpExpr op e1 e2' BoolType)
        | otherwise = err

clause :: (Data body, Typeable body)
          => Clause Expr -> TypingBody body (Clause TExpr)
clause (Clause n e) =
  Clause n <$> typeOfExprIs boolType e

typeOfExpr :: (Data body, Typeable body)
           => Expr -> TypingBody body TExpr
typeOfExpr e = setPosition (position e) 
               (catchError (expr (contents e))
                           (\str -> throwError $ 
                                    str ++ " at " ++ show (position e)))

typeOfExprIs :: (Data body, Typeable body)
                => Typ -> Expr -> TypingBody body TExpr
typeOfExprIs typ e = do
  e' <- typeOfExpr e
  _  <- guardTypeIs typ e'
  return e'

expr :: (Data body, Typeable body)
     => UnPosExpr -> TypingBody body TExpr
expr (LitInt i)    = tagPos (T.LitInt i AnyIntType)
expr (LitDouble d) = tagPos (T.LitDouble d)
expr (LitBool b)   = tagPos (T.LitBool b)
expr LitVoid       = tagPos (T.LitVoid VoidType)
expr (LitString s) = tagPos (T.LitString s)
expr (LitChar c)   = tagPos (T.LitChar c)
expr CurrentVar    = currentM
expr ResultVar     = join (tagPos <$> T.ResultVar <$> result <$> ask)

expr (StaticCall typ name args) = do
  flatClas <- getFlat' typ
  case findAbsRoutine flatClas name of
    Nothing -> 
      throwError (show typ ++ ": does not contain static call " ++ 
                  Text.unpack name)
    Just feat-> do
      args' <- mapM typeOfExpr args
      let argTypes = map declType (routineArgs feat)
      args'' <- argsConform args' argTypes
      tagPos (T.StaticCall typ name args'' (routineResult feat))

expr (Attached typeMb attch asMb) = do
  --TODO: Decide if we have to do any checking between typeMb and attch
  attch' <- typeOfExpr attch
  tagPos $ T.Attached typeMb attch' asMb

expr (UnOpExpr op e)
  | op == Old = do
    e' <- typeOfExpr e
    tagPos (T.Old e')
  | otherwise = do
    e' <- typeOfExpr e
    tagPos (T.UnOpExpr op e' (T.texpr e'))

expr (BinOpExpr op e1 e2) =
  do e1' <- typeOfExpr e1
     e2' <- typeOfExpr e2
     checkBinOp op e1' e2'

expr (UnqualCall fName args) = do
  currTypEi <- current <$> ask
  case currTypEi of
    Right _t ->
      do qual <- QualCall <$> tagPos CurrentVar 
                          <*> pure fName 
                          <*> pure args
         expr qual
    Left t -> expr (StaticCall t fName args) 

expr (QualCall trg name args) = do
  trg' <- typeOfExpr trg
  let targetType = T.texpr trg'
  args' <- mapM typeOfExpr args

  when (isBasic targetType)
       (throwError "Qualified call on basic type")

  -- instantiated class 
  instClass  <- resolveIFace targetType

  case findAbsRoutine instClass name of
    Nothing ->
      case findAttrInt instClass name of
        Just a ->
          do let resType = declType $ attrDecl a
             access <- tagPos $ T.Access trg' name resType
             castResult trg' access resType resType
        Nothing -> throwError $
                   concat ["expr.QualCall: ", show trg, ": "
                          , Text.unpack name, show args'
                          , show (map (routineName) $ view routines instClass)
                          ]
    Just rout -> do
      origClass <- getFlat' targetType
       
      let formArgs = map declType (routineArgs rout)
          resType = routineResult rout
          Just routn = findAbsRoutine origClass name
          originalResType = routineResult routn
          originalArgTypes = map declType (routineArgs routn)
      castedArgs <- argsConform args' originalArgTypes
      baseCall <- tagPos (T.Call trg' name castedArgs resType)
      castResult trg' baseCall originalResType resType

expr (CreateExpr typ name args) = do
  -- this comes basically from the above 
  -- see if the call is valid wth the args proposed
  flatCls <- getFlat' typ
  
  case findAbsRoutine flatCls name of
    Nothing -> throwError $ "expr:CreateExpr no procedure " ++ Text.unpack name
    Just feat -> do
      -- typecheck and cast the arguments if they come from a generic class
      args'   <- mapM typeOfExpr args
      argsConform args' (map declType (routineArgs feat))
      tagPos (T.CreateExpr typ name args')

expr (Lookup targ args) = do
  targ' <- typeOfExpr targ
  cls <- getFlat' (T.texpr targ')
  case findOperator cls "[]" (length args) of
    Nothing -> throwError $ 
      "expr.BinOp.Lookup: [] not found in " ++ show (T.texpr targ')
    Just feat -> expr $ QualCall targ (routineName feat) args

expr (VarOrCall s) =
  do tyMb <- typeOfVar s
     case tyMb of
       Just ty -> tagPos (T.Var s ty)
       Nothing ->
         do currEi <- current <$> ask
            case currEi of
              Left _ -> throwError $
                        "TypeCheck.Expr.expr: var or attribute not found: " ++ Text.unpack s
              Right currType ->
                do curr <- tagPos (T.CurrentVar currType)
                   currCls <- getFlat' (T.texpr curr)
                   case findAttrInt currCls s of
                     Just a ->
                       do let resType = declType $ attrDecl a
                          access <- tagPos $ T.Access curr s resType
                          castResult curr access resType resType
                     Nothing ->
                       throwError $
                         "TypeCheck.Expr.expr: var or attribute not found: " ++ Text.unpack s

expr t = throwError ("TypeCheck.Expr.expr: " ++ show t)

-- | Based on target and result type cast the result to separate
-- or to the appropriate generic
castResult target originalExpr originalResType resultType =
  case targetType of
    -- FIXME: Separate target with a generic parameter?
    Sep _ _ _ ->
      if not (isBasic resultType) && not (isSeparate resultType)
      then tagPos (T.InheritProc target originalExpr)
      else return originalExpr
    _ -> if isAnyRefType originalResType
         then tagPos (T.Cast resultType originalExpr)
         else return originalExpr
  where
    targetType = T.texpr target

-- | Checks that a list of args conform to a list of types. Raises an error
-- if the check fails.
argsConform :: [TExpr]       -- ^ List of typed expressions
               -> [Typ]      -- ^ List of types that the argument list should
                             --   conform to.
               -> TypingBody body [TExpr]
argsConform args formArgs
    | length args == length formArgs =
        zipWithM checkArg args formArgs
    | otherwise = 
        throwError $ "Argument types differ: " ++ show (args, formArgs)
    where
      checkArg e typ 
          | eType == typ = return e
          | eType == AnyIntType && isIntegerType typ
              = tagPos (T.Cast typ e)
          | not (isBasic eType) && isAnyRefType typ
              = tagPos (T.Cast typ e)
          | not (isBasic typ) && isAnyRefType eType
              = tagPos (T.Cast typ e)
          | not (isBasic eType || isSeparate eType) && isSeparate typ
              = tagPos (T.CurrSep e)
          | otherwise =
              throwError $ "Argument type doesn't match: " ++ show (e, typ)
          where
            eType = T.texpr e
