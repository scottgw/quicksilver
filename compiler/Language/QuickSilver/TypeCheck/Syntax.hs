{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}


module Language.QuickSilver.TypeCheck.Syntax where

import           Control.Applicative
import           Control.Monad
import           Control.Lens

import           Data.Int
import qualified Data.Map as Map
import           Data.Map (Map)
import           Data.Maybe
import qualified Data.Text.Encoding as Text
import           Data.Text (Text)

import           Language.QuickSilver.Position
import qualified Language.QuickSilver.Syntax as U
import qualified Language.QuickSilver.Parser as U

type ClassName = Text

classStr :: Text
classStr = "class A fib (i: Integer): Integer do if i <= 1 then Result := 1 else Result := fib (i - 1) + fib (i - 2) end end end"
classAst =
  case U.parseClass classStr of
    Left err -> error $ show err
    Right clas -> clas

checkAll :: U.Clas -> Maybe (Witness Class)
checkAll clas = typeCheckClass (mkClassEnv [clas]) clas

isIntOp = isJust

type SymbolEnv = Map Text ETyp
type ClassEnv = Map Text EClassType

mkClassEnv :: [U.Clas] -> ClassEnv
mkClassEnv = foldr insertClass Map.empty
  where
    insertClass :: U.Clas -> ClassEnv -> ClassEnv
    insertClass uclass =
      Map.insert (view U.className uclass) (uclassToClassType uclass)

    uclassToClassType :: U.Clas -> EClassType
    uclassToClassType uclass = funcPart
      where
        attrPart =
          foldr addAttr
                (EClassType $ ClassNilType (view U.className uclass))
                (view U.attributes uclass)

        addAttr :: U.Attribute -> EClassType -> EClassType
        addAttr attr (EClassType class_) =
          let U.Decl name uty = U.attrDecl attr
          in etypMap (\ty -> EClassType (AddAttrType name ty class_))
                     (utyToTy uty)
        funcPart = foldr addFunc attrPart (view U.routines uclass)

        addFunc func (EClassType class_) =
          case mkFuncType of
            ETyp fType ->
              EClassType (AddFuncType (U.routineName func)  fType class_)
          where
            resultUType = U.routineResult func
            args = U.routineArgs func
            mkFuncType = foldr addArg (utyToTy resultUType) args

            addArg :: U.Decl -> ETyp -> ETyp
            addArg decl (ETyp funcType) =
              case utyToTy (U.declType decl) of
                ETyp argType -> ETyp (FunType argType funcType)

typeCheckClass :: ClassEnv -> U.Clas -> Maybe (Witness Class)
typeCheckClass classEnv uclass = funcPart
  where
    className = uclass ^. U.className
    attrs = uclass ^. U.attributes

    attrPart :: Witness Class
    attrPart =
      foldr addAttr (ClassNil className :^^ Class className) attrs

    -- | FIXME: Lift this higher, add to typed AST module.
    -- It rearranges the order of arguments a bit to work better with
    -- etypMap
    -- | FIXME: Replace the `Text -> Typ a` arguments with a typed `Decl a`
    addAttr' :: Witness Class -> Text -> Typ a -> Witness Class
    addAttr' (class_ :^^ Class classType) name ty =
      AddAttr name ty class_ :^^ Class classType

    addAttr attr classWitness =
      etypMap (addAttr' classWitness name) (utyToTy type_)
      where
        U.Decl name type_ = U.attrDecl attr

    funcs = uclass ^. U.routines

    funcPart :: Maybe (Witness Class)
    funcPart = foldM addFunc attrPart funcs

    -- | FIXME: Lift this higher, add to typed AST module.
    -- It rearranges the order of arguments a bit to work better with
    -- etypMap
    addFunc' :: Witness Class -> Text -> Func a -> Typ a -> Witness Class
    addFunc' (class_ :^^ Class classType) name func ty =
      AddFunc name func class_ :^^ Class classType

    addFunc :: Witness Class -> U.Routine -> Maybe (Witness Class)
    addFunc classWitness func =
      do func' :^^ funcType <- typeCheckRoutine currentType  classEnv func
         return (addFunc' classWitness name func' funcType)
               where
                 currentType = Class className
                 name = U.routineName func

typeCheckExpr :: Typ a -> ClassEnv -> SymbolEnv -> U.Expr -> Maybe (Witness Expr)
typeCheckExpr currentType classEnv symbolEnv e =
  case contents e of

    U.BinOpExpr op e1 e2 ->
      case asIntIntOp op of
        Just op' -> 
          do e1' :^^ IntType <- checkExpr e1
             e2' :^^ IntType <- checkExpr e2
             return (IntIntExpr op' e1' e2' :^^ IntType)
        Nothing ->
          case asIntBoolOp op of
            Just op' ->
              do e1' :^^ IntType <- checkExpr e1
                 e2' :^^ IntType <- checkExpr e2
                 return (IntBoolExpr op' e1' e2' :^^ BoolType)
    U.LitInt i ->
      return (LitInt (fromIntegral i) :^^ IntType)

    U.VarOrCall var ->
      do ETyp t <- Map.lookup var symbolEnv
         return (VarExpr var :^^ t)

    U.CurrentVar ->
      return (CurrentVar :^^ currentType)

    U.UnqualCall name args ->
      checkExpr (takePos e (U.QualCall (takePos e U.CurrentVar) name args))

    U.QualCall trg name args ->
      do trg':args' <- mapM checkExpr (trg:args)
         _  :^^ Class className <- return trg'
         ETyp funcType <- findMember className name
         checkFunc (FuncExpr name) funcType (trg':args')

    _ -> error ("typeCheckExpr: unknown case " ++ show e)

  where
    findMember :: Text -> Text -> Maybe ETyp
    findMember className name =
      do EClassType iface  <- Map.lookup className classEnv
         case iface of
           ClassNilType _ -> Nothing
           AddAttrType attrName type_ rest
             | attrName == name -> return (ETyp type_)
             | otherwise -> Nothing
           AddFuncType funcName type_ rest
             | funcName == name -> return (ETyp type_)
             | otherwise -> Nothing

    
    checkExpr = typeCheckExpr currentType classEnv symbolEnv

    checkFunc :: Expr a -> Typ a -> [Witness Expr] -> Maybe (Witness Expr)
    checkFunc result resultType [] = return (result :^^ resultType)
    checkFunc funcExpr funcType ( (arg :^^ argType) : args ) =
      case funcType of
        FunType funcArgType rest ->
          do Eq <- test funcArgType argType
             let call = ApplyExpr funcExpr arg
             checkFunc call rest args
        _ -> Nothing

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


data Witness f where
  (:^^) :: Show (f a) => f a -> Typ a -> Witness f

data Equal a b where
  Eq :: Equal a a

test :: Typ a -> Typ b -> Maybe (Equal a b)
test IntType IntType   = return Eq
test BoolType BoolType = return Eq
test (FunType a b) (FunType a' b') =
  do Eq <- test a a'
     Eq <- test b b'
     return Eq
test _ _               = Nothing

typeCheckStmt :: Typ a -> ClassEnv -> SymbolEnv -> Typ b -> U.Stmt -> Maybe (Stmt b)
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
      case contents loc of
        U.ResultVar ->
          do expr' :^^ t <- checkExpr expr
             Eq <- test t resultType
             return (ResultAssign expr')

    U.CallStmt e ->
      do e' :^^ _ <- checkExpr e
         return (CallStmt e')

  where
    checkExpr = typeCheckExpr currentType classEnv symbolEnv
    checkStmt = typeCheckStmt currentType classEnv symbolEnv resultType

typeCheckRoutine :: Typ a -> ClassEnv -> U.Routine -> Maybe (Witness Func)
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

data ETyp = forall a . ETyp (Typ a)

etypMap :: (forall a. Typ a -> b) -> ETyp -> b
etypMap f (ETyp x) = f x

utyToTy :: U.Typ -> ETyp
utyToTy uty = ETyp $
  case uty of
    U.Int64Type -> IntType

data IntIntOp
  = Add
  | Sub
  | Mul
  | Div

data IntBoolOp
  = Lt
  | Lte

data Expr a where
  CurrentVar :: Expr a
  LitInt :: Int -> Expr Int
  IntBoolExpr :: IntBoolOp -> Expr Int -> Expr Int -> Expr Bool
  IntIntExpr :: IntIntOp -> Expr Int -> Expr Int -> Expr Int
  FuncExpr :: Text -> Expr a
  ApplyExpr :: Expr (a -> b) -> Expr a -> Expr b
  VarExpr :: Text -> Expr a

data Stmt a where
  If :: Expr Bool -> Stmt a -> Maybe (Stmt a) -> Stmt a
  Block :: [Stmt a] -> Stmt a
  ResultAssign :: Expr a -> Stmt a
  CallStmt :: Expr a -> Stmt b

data Func a where
  FuncBody :: Text -> Typ a -> Stmt a -> Func a
  AddArg :: Text -> Typ a -> Func b -> Func (a -> b)

funcToType :: Func a -> Typ a
funcToType func =
  case func of
    FuncBody name resultType stmt -> resultType
    AddArg name type_ func -> FunType type_ (funcToType func)

data Class a where
  ClassNil :: Text -> Class ()
  AddAttr :: Text -> Typ a -> Class rest -> Class (a, rest)
  AddFunc :: Text -> Func a -> Class rest -> Class (a, rest)

data EClassType = forall cls . EClassType (ClassIFace cls)

data ClassIFace a where
  ClassNilType :: Text -> ClassIFace ()
  AddAttrType :: Text -> Typ a -> ClassIFace rest -> ClassIFace (a, rest)
  AddFuncType :: Text -> Typ a -> ClassIFace rest -> ClassIFace (a, rest)

classToIFace :: Class a -> ClassIFace a
classToIFace class_ =
  case class_ of
    ClassNil name -> ClassNilType name
    AddAttr name type_ c -> AddAttrType name type_ (classToIFace c)
    AddFunc name func c -> AddFuncType name (funcToType func) (classToIFace c)

classTypeName :: ClassIFace a -> Text
classTypeName iface =
  case iface of
    ClassNilType t -> t
    AddAttrType _ _ c -> classTypeName c
    AddFuncType _ _ c -> classTypeName c

data Typ a where
  TupType :: Typ a -> Typ b -> Typ (a, b)
  Class :: Text -> Typ a
  FunType :: Typ a -> Typ b -> Typ (a -> b)
  IntType :: Typ Int
  BoolType :: Typ Bool
  NoType :: Typ ()

deriving instance Show EClassType
deriving instance Show (Witness f)
deriving instance Show IntIntOp
deriving instance Show IntBoolOp
deriving instance Show (Expr a)
deriving instance Show (Stmt a)
deriving instance Show (Func a)
deriving instance Show (Class a)
deriving instance Show (ClassIFace a)
deriving instance Show (Typ a)

         -- | AnyIntType
         -- | Int8Type
         -- | Int16Type
         -- | Int32Type
         -- | Int64Type
         -- | BoolType
         -- | DoubleType
         -- | CharType
         -- | ProcessorType
         -- | Natural8Type
         -- | Natural16Type
         -- | Natural32Type
         -- | Natural64Type
         -- | AnyRefType Text
         -- | Sep (Maybe Proc) [Proc] Typ
         -- | VoidType
         -- | NoType deriving (Eq, Ord, G.Generic, D.Data, T.Typeable)
