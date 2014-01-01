{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
module Language.QuickSilver.TypeCheck.Syntax where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans.Identity
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Error
import           Control.Lens

import           Data.Functor.Identity
import           Data.Int
import qualified Data.Map as Map
import           Data.Map (Map)
import           Data.Maybe
import qualified Data.Text as Text
import           Data.Text (Text)

import           Language.QuickSilver.Position
import qualified Language.QuickSilver.Syntax as U
import qualified Language.QuickSilver.Parser as U

type ClassName = Text

type TC a = ErrorT String Identity a

tcLookup :: (Ord key, Show key) => key -> Map key value -> TC value
tcLookup key m =
  case Map.lookup key m of
    Nothing -> throwError $ "tcLookup: couldn't find " ++ show key
    Just value -> return value

classStr :: Text
classStr = "class A fib (i: Integer): Integer do if i <= 1 then Result := 1 else Result := fib (i - 1) + fib (i - 2) end end end"
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

-- newClassId :: ClassEnv -> ENat
-- newClassId m = Map.foldr go (ENat ZeroN) m
--   where
--     go (EClassIFace iface) (ENat m) =
--       case ifaceType iface of
--         ETyp (Class _ n) -> maxNat n m

--     maxNat :: Nat a -> Nat b -> ENat
--     maxNat (SuccN n) (SuccN m) =
--       case maxNat n m of
--         ENat n' -> ENat (SuccN n')
--     maxNat ZeroN     n         = ENat n
--     maxNat n         ZeroN     = ENat n

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
    addFunc' (class_ :^^ Class classType n) name func ty =
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
      do ETyp t <- tcLookup var symbolEnv
         return (VarExpr var :^^ t)

    U.CurrentVar ->
      return (CurrentVar :^^ currentType)

    U.UnqualCall name args ->
      checkExpr (takePos e (U.QualCall (takePos e U.CurrentVar) name args))

    U.QualCall trg name args ->
      do trg':args' <- mapM checkExpr (trg:args)
         _  :^^ Class className n <- return trg'
         ETyp funcType <- findMember className name
         checkFunc (FuncExpr name) funcType (trg':args')

    _ -> error ("typeCheckExpr: unknown case " ++ show e)

  where
    findMember :: Text -> Text -> TC ETyp
    findMember className name =
      do EClassIFace iface <- tcLookup className classEnv
         case iface of
           AddAttrType attrName type_ rest
             | attrName == name -> return (ETyp type_)
           AddFuncType funcName type_ rest
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


data Witness f where
  (:^^) :: Show (f a) => f a -> Typ a -> Witness f

data Equal a b where
  Eq :: Equal a a

natEq :: Nat a -> Nat b -> TC (Equal a b)
natEq ZeroN ZeroN = return Eq
natEq (SuccN n) (SuccN m) =
  do Eq <- natEq n m
     return Eq
natEq n m = throwError "natEq: unequal"

test :: Typ a -> Typ b -> TC (Equal a b)
test IntType IntType   = return Eq
test BoolType BoolType = return Eq
test (Class a n) (Class b m) =
  do Eq <- natEq n m
     return Eq
test (FunType a b) (FunType a' b') =
  do Eq <- test a a'
     Eq <- test b b'
     return Eq
test a b               =
  throwError ("test: couldn't match " ++ show a ++ " and " ++ show b)

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
  ClassNil :: Text -> Nat a -> Class a
  AddAttr :: Text -> Typ a -> Class rest -> Class rest
  AddFunc :: Text -> Func a -> Class rest -> Class rest

data EClassIFace = forall cls . EClassIFace (ClassIFace cls)

data ClassIFace a where
  ClassNilType :: Text -> Nat a -> ClassIFace a
  AddAttrType :: Text -> Typ a -> ClassIFace rest -> ClassIFace rest
  AddFuncType :: Text -> Typ a -> ClassIFace rest -> ClassIFace rest

ifaceType :: ClassIFace a -> ETyp
ifaceType iface =
  case iface of
    ClassNilType name n -> ETyp (Class name n)
    AddAttrType _ _ c -> ifaceType c
    AddFuncType _ _ c -> ifaceType c


classToIFace :: Class a -> ClassIFace a
classToIFace class_ =
  case class_ of
    ClassNil name n -> ClassNilType name n
    AddAttr name type_ c -> AddAttrType name type_ (classToIFace c)
    AddFunc name func c -> AddFuncType name (funcToType func) (classToIFace c)

classTypeName :: ClassIFace a -> Text
classTypeName iface =
  case iface of
    ClassNilType t _ -> t
    AddAttrType _ _ c -> classTypeName c
    AddFuncType _ _ c -> classTypeName c

data Nat a where
  ZeroN :: Nat Zero
  SuccN :: Nat a -> Nat (Succ a)

deriving instance Show (Nat a)

data Succ a
data Zero

data Typ a where
  TupType :: Typ a -> Typ b -> Typ (a, b)
  Class :: Text -> Nat a -> Typ a
  FunType :: Typ a -> Typ b -> Typ (a -> b)
  IntType :: Typ Int
  BoolType :: Typ Bool
  NoType :: Typ ()

deriving instance Show EClassIFace
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
