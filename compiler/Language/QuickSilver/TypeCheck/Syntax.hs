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
