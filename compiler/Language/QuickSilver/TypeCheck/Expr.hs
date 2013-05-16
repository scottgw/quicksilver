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

import           Data.Maybe
import qualified Data.Map as Map
import qualified Data.Text as Text
import           Data.Text (Text)

import           Language.QuickSilver.Syntax
import           Language.QuickSilver.Position
import           Language.QuickSilver.Util
import qualified Language.QuickSilver.TypeCheck.TypedExpr as T
import           Language.QuickSilver.TypeCheck.TypedExpr (TExpr)
import           Language.QuickSilver.TypeCheck.BasicTypes
import           Language.QuickSilver.TypeCheck.Context
import           Language.QuickSilver.TypeCheck.Generic

import           Util.Monad

checkBinOp :: BinOp -> TExpr -> TExpr -> TypingBody body TExpr
checkBinOp op e1 e2 = 
    case lookup (op, t1, t2) opList of
      Just resType -> tagPos (T.BinOpExpr op e1 e2 resType)
      Nothing -> 
          throwError $ concat ["checkBinOp: no infix operation for: "
                              , show (op, t1, t2)
                              ]
    where
      t1 = T.texpr e1
      t2 = T.texpr e2

      opList =
          [ ((Add, IntType, IntType), IntType)
          , ((RelOp Gt NoType, IntType, IntType), BoolType)
          ]

clause :: Clause Expr -> TypingBody body (Clause TExpr)
clause (Clause n e) =
  Clause n <$> typeOfExprIs boolType e

maybeTag :: Maybe a -> TypingBody ctxBody (Maybe (Pos a))
maybeTag Nothing  = return Nothing
maybeTag (Just x) = Just <$> tagPos x

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
expr (LitInt i)    = tagPos (T.LitInt i)
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
      argsConform args' argTypes
      tagPos (T.StaticCall typ name args' (routineResult feat))

expr (Attached typeMb attch asMb) = do
  --TODO: Decide if we have to do any checking between typeMb and attch
  attch' <- typeOfExpr attch
  curr <- current <$> ask
  -- FIXME: we don't need the below do we?
  -- flatClas <- getFlat' curr
  tagPos $ T.Attached typeMb attch' asMb

expr (UnOpExpr op e)
  | op == Old = do
    e' <- typeOfExpr e
    tagPos (T.Old e')
  | otherwise = do
    e' <- typeOfExpr e
    cls <- getFlat' (T.texpr e')
    case findOperator cls (unOpAlias op) 0 of
      Nothing -> 
        throwError 
        ("No routine found associated with unary operator " ++ show op ++ " in " ++ show (T.texpr e'))
      Just f -> expr (QualCall e (routineName f) [])

expr (BinOpExpr op e1 e2) 
  | equalityOp op = do
    e1' <- typeOfExpr e1
    e2' <- typeOfExpr e2
    tagPos (T.EqExpr (T.eqOp op) e1' e2')
  | otherwise = do
    e1' <- typeOfExpr e1
    e2' <- typeOfExpr e2
    let t1 = T.texpr e1'
        t2 = T.texpr e2'
    checkBinOp op e1' e2'

expr (UnqualCall fName args) = do
  qual <- QualCall <$> tagPos CurrentVar 
                   <*> pure fName 
                   <*> pure args
  expr qual

expr (QualCall trg name args) = do
  trg' <- typeOfExpr trg
  let targetType = T.texpr trg'
  args' <- mapM typeOfExpr args
  flatCls  <- getFlat' targetType

  case findAbsRoutine flatCls name of
    Nothing -> throwError $ "expr.QualCall: " ++ show trg ++ ": " ++ Text.unpack name ++ show args' ++ show (map (routineName) $ view routines flatCls)
    Just feat -> do
      let formArgs = map declType (routineArgs feat)
          res = routineResult feat
      catchError 
         (do argsConform args' formArgs
             tagPos (T.Call trg' name args' res)
         )
         (\e -> case args' of
             [arg] -> 
               if contents arg `numericCanBe` targetType
               then do -- check to see if the numeric types can 
                       -- be casted to one another
                 arg' <- tagPos (T.Cast targetType arg)
                 tagPos (T.Call trg' name [arg'] res)
               else if contents trg' `numericCanBe` T.texpr arg 
                    then do 
                      trg'' <- tagPos (T.Cast (T.texpr arg) trg')
                      tagPos (T.Call trg'' name [arg] res)
                    else throwError $ e ++ " AND! " ++ show (trg',name,args, targetType)
             _ -> throwError e
         )
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
expr (LitType t) = tagPos (T.LitType t)
expr (ManifestCast t e) = do
  e' <- typeOfExpr e
  tagPos (T.Cast t e')
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
               -> TypingBody body ()
argsConform args formArgs 
    | map T.texpr args == formArgs = return ()
    | otherwise = 
        throwError $ "Differing number of args: " ++ show (args, formArgs)
