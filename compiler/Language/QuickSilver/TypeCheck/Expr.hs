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

import qualified Data.Text as Text

import           Language.QuickSilver.Syntax
import           Language.QuickSilver.Position
import           Language.QuickSilver.Util
import qualified Language.QuickSilver.TypeCheck.TypedExpr as T
import           Language.QuickSilver.TypeCheck.TypedExpr (TExpr)
import           Language.QuickSilver.TypeCheck.BasicTypes
import           Language.QuickSilver.TypeCheck.Context

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
      isNumOp  = op `elem` [Add, Sub, Mul, Div]
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

clause :: Clause Expr -> TypingBody body (Clause TExpr)
clause (Clause n e) =
  Clause n <$> typeOfExprIs boolType e

typeOfExpr :: Expr -> TypingBody body TExpr
typeOfExpr e = setPosition (position e) 
               (catchError (expr (contents e))
                           (\str -> throwError $ 
                                    str ++ " at " ++ show (position e)))

typeOfExprIs :: Typ -> Expr -> TypingBody body TExpr
typeOfExprIs typ e = do
  e' <- typeOfExpr e
  _  <- guardTypeIs typ e'
  return e'

expr :: forall body . UnPosExpr -> TypingBody body TExpr
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
  flatCls  <- getFlat' targetType

  case findAbsRoutine flatCls name of
    Nothing ->
      case findAttrInt flatCls name of
        Just a -> tagPos (T.Access trg' name (declType $ attrDecl a))
        Nothing -> throwError $
                   concat ["expr.QualCall: ", show trg, ": "
                          , Text.unpack name, show args'
                          , show (map (routineName) $ view routines flatCls)
                          ]
    Just feat -> do
      let formArgs = map declType (routineArgs feat)
          resType = routineResult feat
      args'' <- argsConform args' formArgs
      baseCall <- tagPos (T.Call trg' name args'' resType)
      case targetType of
        Sep _ _ _ ->
            if not (isBasic resType)
            then tagPos (T.InheritProc trg' baseCall)
            else return baseCall
        _ -> return baseCall

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
         do !curr <- currentM
            !currCls <- getFlat' (T.texpr curr)
            case findAttrInt currCls s of
              Just a -> tagPos (T.Access curr s (declType $ attrDecl a))
              Nothing ->
                throwError "TypeCheck.Expr.expr: var or attribute not found"
expr t = throwError ("TypeCheck.Expr.expr: " ++ show t)

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
          | T.texpr e == typ = return e
          | T.texpr e == AnyIntType && isIntegerType typ
              = tagPos (T.Cast typ e)
          | otherwise = throwError $ "Argument type doesn't match: " ++ show (e, typ)
