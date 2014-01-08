{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Language.QuickSilver.TypeCheck.Check where

import           Control.Lens hiding (op)
import           Control.Monad.Error

import           Data.Map.Lazy (Map)
import qualified Data.Map.Lazy as Map
import           Data.Text (Text)
import qualified Data.Text as Text

import           Language.QuickSilver.TypeCheck.Syntax
import           Language.QuickSilver.Position
import qualified Language.QuickSilver.Syntax as U
import qualified Language.QuickSilver.Parser as U
import           Control.Applicative

type TC a = ErrorT String Identity a

tcLookup :: (Ord key, Show key) => key -> Map key value -> TC value
tcLookup key m =
  case Map.lookup key m of
    Nothing -> throwError $ "tcLookup: couldn't find " ++ show key
    Just value -> return value

classStr :: Text
classStr = "class A fib (i: Integer): Integer do if i <= 1 then Result := 1 else Result := fib (i - 1) + fib (i - 2) end end end"

classAst :: U.Clas
classAst =
  case U.parseClass classStr of
    Left err -> error $ show err
    Right clas -> clas

checkAll' :: U.Clas -> Either String (Witness Class)
checkAll' clas =
  runIdentity $ runErrorT $ typeCheckClass (mkClassEnv [clas]) clas

checkAll :: U.Clas -> TC (Witness Class)
checkAll clas = typeCheckClass (mkClassEnv [clas]) clas

type SymbolEnv = Map Text ETyp
type ClassEnv = Map Text EClassIFace

data ENat = forall a . ENat (Nat a)

mkClassEnv :: [U.Clas] -> ClassEnv
mkClassEnv = snd . foldr insertClass (ENat ZeroN, Map.empty)
  where
    insertClass :: U.Clas -> (ENat, ClassEnv) -> (ENat, ClassEnv)
    insertClass uclass (ENat freeId, m) =
      (ENat (SuccN freeId),
       Map.insert (uclass ^. U.className) (uclassToClassType freeId uclass) m
      )

    uclassToClassType :: Nat a -> U.Clas -> EClassIFace
    uclassToClassType freeId uclass = funcPart
      where
        className = uclass ^. U.className

        attrPart =
          foldr addAttr
                (EClassIFace $ ClassNilType className freeId)
                (view U.attributes uclass)

        addAttr :: U.Attribute -> EClassIFace -> EClassIFace
        addAttr attr (EClassIFace class_) =
          let U.Decl name uty = U.attrDecl attr
          in etypMap (\ty -> EClassIFace (AddAttrType name ty class_))
                     (utyToTy uty)
        funcPart = foldr addFunc attrPart (view U.routines uclass)

        addFunc func (EClassIFace class_) =
          case mkFuncType of
            ETyp fType ->
              EClassIFace (AddFuncType (U.routineName func)  fType class_)
          where

            resultUType = U.routineResult func
            args = U.routineArgs func

            mkFuncType = addFnArg (foldr addArg (utyToTy resultUType) args)
                                  (Class className freeId)

            addFnArg :: ETyp -> Typ a -> ETyp
            addFnArg (ETyp result) t = ETyp (FunType t result)

            addArg :: U.Decl -> ETyp -> ETyp
            addArg decl resultType =
              etypMap (addFnArg resultType) (utyToTy (U.declType decl))


typeCheckClass :: ClassEnv -> U.Clas -> TC (Witness Class)
typeCheckClass classEnv uclass =
  do EClassIFace iface <- tcLookup (uclass ^. U.className) classEnv
     ETyp (Class _ classId) <- return (ifaceType iface)
     typeCheckClass' classEnv classId uclass

typeCheckClass' :: ClassEnv -> Nat a -> U.Clas -> TC (Witness Class)
typeCheckClass' classEnv classId uclass = funcPart
  where
    className = uclass ^. U.className
    attrs = uclass ^. U.attributes

    attrPart :: Witness Class
    attrPart =
      foldr addAttr (ClassNil className classId :^^ Class className classId) attrs

    -- | FIXME: Lift this higher, add to typed AST module.
    -- It rearranges the order of arguments a bit to work better with
    -- etypMap
    -- | FIXME: Replace the `Text -> Typ a` arguments with a typed `Decl a`
    addAttr' :: Witness Class -> Text -> Typ a -> Witness Class
    addAttr' (class_ :^^ classType) name ty =
      AddAttr name ty class_ :^^ classType

    addAttr attr classWitness =
      etypMap (addAttr' classWitness name) (utyToTy type_)
      where
        U.Decl name type_ = U.attrDecl attr

    funcs = uclass ^. U.routines

    funcPart :: TC (Witness Class)
    funcPart = foldM addFunc attrPart funcs

    -- | FIXME: Lift this higher, add to typed AST module.
    -- It rearranges the order of arguments a bit to work better with
    -- etypMap
    addFunc' :: Witness Class -> Text -> Func a -> Typ a -> Witness Class
    addFunc' (class_ :^^ Class classType n) name func _ty =
      AddFunc name func class_ :^^ Class classType n

    addFunc :: Witness Class -> U.Routine -> TC (Witness Class)
    addFunc classWitness func =
      do EClassIFace currentIFace <- tcLookup className classEnv
         ETyp currentType <- return (ifaceType currentIFace)
         func' :^^ funcType <- typeCheckRoutine currentType classEnv func
         return (addFunc' classWitness name func' funcType)
           where
             name = U.routineName func

typeCheckExpr :: Typ a -> ClassEnv -> SymbolEnv -> U.Expr -> TC (Witness Expr)
typeCheckExpr currentType classEnv symbolEnv e =
  case contents e of

    U.BinOpExpr op e1 e2 ->
      case asIntIntOp op of
        Just op' -> 
          do e1' :^^ t1 <- checkExpr e1
             e2' :^^ t2 <- checkExpr e2
             Eq <- test t1 t2
             IsWhole <- isWhole t1
             return (IntIntExpr op' e1' e2' :^^ t1)
        Nothing ->
          case asIntBoolOp op of
            Just op' ->
              do e1' :^^ t1 <- checkExpr e1
                 e2' :^^ t2 <- checkExpr e2
                 Eq <- test t1 t2
                 IsWhole <- isWhole t1
                 return (IntBoolExpr op' e1' e2' :^^ BoolType)
            Nothing -> throwError $ "typeCheckExpr: doesn't handle op: " ++ show op

    U.LitInt i ->
      return (LitInt (fromIntegral i) :^^ IntegerType)

    U.VarOrCall var ->
      do ETyp t <- tcLookup var symbolEnv
         return (VarExpr var :^^ t)

    U.CurrentVar ->
      return (CurrentVar :^^ currentType)

    U.UnqualCall name args ->
      checkExpr (takePos e (U.QualCall (takePos e U.CurrentVar) name args))

    U.QualCall trg name args ->
      do trg':args' <- mapM checkExpr (trg:args)
         _  :^^ Class className _n <- return trg'
         ETyp funcType <- findMember className name
         checkFunc (FuncExpr name) funcType (trg':args')

    _ -> error ("typeCheckExpr: unknown case " ++ show e)

  where
    findMember :: Text -> Text -> TC ETyp
    findMember className name =
      do EClassIFace iface <- tcLookup className classEnv
         case iface of
           AddAttrType attrName type_ _rest
             | attrName == name -> return (ETyp type_)
           AddFuncType funcName type_ _rest
             | funcName == name -> return (ETyp type_)
           _ -> throwError $ "findMember: couldn't find member " ++ Text.unpack name ++ " in " ++ show iface
           -- ClassNilType _ -> Nothing
           --   | otherwise -> Nothing
           
           --   | otherwise -> Nothing
    
    checkExpr = typeCheckExpr currentType classEnv symbolEnv

    checkFunc :: Expr a -> Typ a -> [Witness Expr] -> TC (Witness Expr)
    checkFunc result resultType [] = return (result :^^ resultType)
    checkFunc funcExpr funcType ( (arg :^^ argType) : args ) =
      case funcType of
        FunType funcArgType rest ->
          do Eq <- test funcArgType argType
             let call = ApplyExpr funcExpr arg
             checkFunc call rest args
        _ -> throwError "checkFun: didn't find function type"

    asIntBoolOp (U.RelOp op _) =
      case op of
        U.Lt -> Just Lt
        U.Lte -> Just Lte
        _ -> Nothing

    asIntIntOp op =
      case op of
        U.Add -> Just Add
        U.Sub -> Just Sub
        U.Mul -> Just Mul
        U.Div -> Just Div
        _ -> Nothing

data IsWhole a where
  IsWhole :: Whole a => IsWhole a

isWhole :: Typ a -> TC (IsWhole a)
isWhole t =
  case t of
    Int8Type -> return IsWhole
    Int16Type -> return IsWhole
    Int32Type -> return IsWhole
    Int64Type -> return IsWhole
    Word8Type -> return IsWhole
    Word16Type -> return IsWhole
    Word32Type -> return IsWhole
    Word64Type -> return IsWhole
    _ -> throwError $ "isn't a natural or integer"

data Witness f where
  (:^^) :: Show (f a) => f a -> Typ a -> Witness f
deriving instance Show (Witness f)

data Equal a b where
  Eq :: Equal a a

natEq :: Nat a -> Nat b -> TC (Equal a b)
natEq ZeroN ZeroN = return Eq
natEq (SuccN n) (SuccN m) =
  do Eq <- natEq n m
     return Eq
natEq _n _m = throwError "natEq: unequal"

test :: Typ a -> Typ b -> TC (Equal a b)
test Int8Type Int8Type   = return Eq
test Int32Type Int32Type = return Eq
test Int64Type Int64Type = return Eq
test BoolType BoolType = return Eq
test (Class _a n) (Class _b m) =
  do Eq <- natEq n m
     return Eq
test (FunType a b) (FunType a' b') =
  do Eq <- test a a'
     Eq <- test b b'
     return Eq
test a b               =
  throwError ("test: couldn't match" ++ show a ++ " and " ++ show b)

typeCheckStmt :: Typ a -> ClassEnv -> SymbolEnv -> Typ b -> U.Stmt -> TC (Stmt b)
typeCheckStmt currentType classEnv symbolEnv resultType stmt =
  case contents stmt of

    U.Block blk -> Block <$> mapM checkStmt blk

    U.If cond thenStmt elseIfs elseStmtMb ->
      do cond' :^^ BoolType <- checkExpr cond
         thenStmt' <- checkStmt thenStmt
         elseStmtMb' <- case elseStmtMb of
           Just elseStmt ->
             do elseStmt' <- checkStmt elseStmt
                return (Just elseStmt')
           Nothing -> 
             return Nothing
         return (If cond' thenStmt' elseStmtMb')

    U.Assign loc expr ->
      do expr' :^^ t <- checkExpr expr
         case contents loc of
           U.ResultVar ->
             do Eq <- test t resultType
                return (ResultAssign expr')
           U.VarOrCall name ->
             do ETyp locType <- tcLookup name symbolEnv
                Eq <- test t locType
                return (Assign name expr')

    U.CallStmt e ->
      do e' :^^ _ <- checkExpr e
         return (CallStmt e')

  where
    checkExpr = typeCheckExpr currentType classEnv symbolEnv
    checkStmt = typeCheckStmt currentType classEnv symbolEnv resultType

typeCheckRoutine :: Typ a -> ClassEnv -> U.Routine -> TC (Witness Func)
typeCheckRoutine currentType classEnv r = argWrapper <$> bodyMb
  where
    bodyMb = case U.routineImpl r of
      U.RoutineBody locals body ->
        do ETyp resTy <- return (utyToTy (U.routineResult r))
           s <- typeCheckStmt currentType classEnv localEnv resTy body
           return (FuncBody (U.routineName r) resTy s :^^ resTy)
        where
          locals' = U.routineArgs r ++ locals
          localEnv = Map.fromList [(n, utyToTy t) | U.Decl n t <- locals']

    argWrapper body =
      case foldr go body (U.routineArgs r) of
        inner :^^ innerType -> AddArg "Current" currentType inner :^^ FunType currentType innerType
    go (U.Decl name uType) (inner :^^ innerType) =
      case utyToTy uType of
        ETyp typ -> AddArg name typ inner :^^ FunType typ innerType

