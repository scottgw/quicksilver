{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
module Language.QuickSilver.TypeCheck.Syntax where

import           Data.Int
import           Data.Word
import           Data.Text (Text)

import qualified Language.QuickSilver.Syntax as U

type ClassName = Text

data ETyp = forall a . ETyp (Typ a)

etypMap :: (forall a. Typ a -> b) -> ETyp -> b
etypMap f (ETyp x) = f x

utyToTy :: U.Typ -> ETyp
utyToTy uty =
  case uty of
    U.BoolType -> ETyp BoolType
    U.AnyIntType -> ETyp IntegerType
    U.Int8Type -> ETyp Int8Type
    U.Int16Type -> ETyp Int16Type
    U.Int32Type -> ETyp Int32Type
    U.Int64Type -> ETyp Int64Type
    U.DoubleType -> ETyp DoubleType
    U.CharType -> ETyp CharType
    U.Natural8Type -> ETyp Word8Type
    U.Natural16Type -> ETyp Word16Type
    U.Natural32Type -> ETyp Word32Type
    U.Natural64Type -> ETyp Word64Type
    U.VoidType -> ETyp VoidType
    U.NoType -> ETyp NoType
    U.Sep baseTy ->
      case utyToTy baseTy of
        ETyp t -> ETyp (Sep t)


data IntIntOp
  = Add
  | Sub
  | Mul
  | Div

data IntBoolOp
  = Lt
  | Lte

-- | Class to tag `whole' number types (integers and naturals).
class Whole a
instance Whole Int8
instance Whole Int16
instance Whole Int32
instance Whole Int64
instance Whole Word8
instance Whole Word16
instance Whole Word32
instance Whole Word64

data Expr a where
  CurrentVar :: Expr a
  LitInt :: Integer -> Expr Integer
  IntBoolExpr :: Whole a => IntBoolOp -> Expr a -> Expr a -> Expr Bool
  IntIntExpr :: Whole a => IntIntOp -> Expr a -> Expr a -> Expr a
  FuncExpr :: Text -> Expr a
  ApplyExpr :: Expr (a -> b) -> Expr a -> Expr b
  VarExpr :: Text -> Expr a

data Stmt a where
  If :: Expr Bool -> Stmt a -> Maybe (Stmt a) -> Stmt a
  Block :: [Stmt a] -> Stmt a
  ResultAssign :: Expr a -> Stmt a
  Assign :: Text -> Expr a -> Stmt b
  CallStmt :: Expr a -> Stmt b

data Func a where
  FuncBody :: Text -> Typ a -> Stmt a -> Func a
  AddArg :: Text -> Typ a -> Func b -> Func (a -> b)

funcToType :: Func a -> Typ a
funcToType func =
  case func of
    FuncBody _name resultType _stmt -> resultType
    AddArg _name type_ base -> FunType type_ (funcToType base)

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

data Sep a
data Void

data Typ a where
  TupType :: Typ a -> Typ b -> Typ (a, b)
  Class :: Text -> Nat a -> Typ a
  FunType :: Typ a -> Typ b -> Typ (a -> b)
  Sep :: Typ a -> Typ (Sep a)
  CharType :: Typ Char 
  DoubleType :: Typ Double
  IntegerType :: Typ Integer
  Int8Type :: Typ Int8
  Int16Type :: Typ Int16
  Int32Type :: Typ Int32
  Int64Type :: Typ Int64
  Word8Type :: Typ Word8
  Word16Type :: Typ Word16
  Word32Type :: Typ Word32
  Word64Type :: Typ Word64
  BoolType :: Typ Bool
  NoType :: Typ ()
  VoidType :: Typ Void

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
