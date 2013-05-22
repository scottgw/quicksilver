{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.QuickSilver.Syntax where

import           Control.DeepSeq
import           Control.Lens hiding (op)

import           Data.Binary
import qualified Data.Data as D
import           Data.DeriveTH
import           Data.Hashable
import           Data.HashMap.Strict (HashMap)
import           Data.List
import qualified Data.Text.Encoding as Text
import           Data.Text (Text)
import qualified Data.Typeable as T

import qualified GHC.Generics as G

import           Language.QuickSilver.Position

type Map = HashMap

newtype Import = Import Text
               deriving (Eq, Show, G.Generic, D.Data, T.Typeable)

type Clas = ClasBody Expr
type ClasBody exp = AbsClas (RoutineBody exp) exp
type ClasInterface = AbsClas EmptyBody Expr
type ClasI exp = AbsClas (RoutineBody exp) exp

data AbsClas body exp =
  AbsClas { _imports    :: [Import]
          , _className  :: ClassName
          , _generics   :: [Generic]
          , _creates    :: [CreateClause]
          , _attributes :: [Attribute]
          , _routines   :: [AbsRoutine body exp]
          , _consts     :: [Constant exp]
          , _invnts     :: [Clause exp]
          , _isModule   :: Bool
          } deriving (Eq, Show, G.Generic, D.Data, T.Typeable)

data Generic = 
  Generic { genericName :: ClassName 
          , genericConstType :: [Typ]
          , genericCreate :: Maybe [Text]
          } deriving (Show, Eq, G.Generic, D.Data, T.Typeable)

data CreateClause = 
  CreateClause { createExportNames :: [ClassName]
               , createNames :: [Text]
               } deriving (Show, Eq, G.Generic, D.Data, T.Typeable)

type RoutineI = AbsRoutine EmptyBody Expr
type RoutineWithBody exp = AbsRoutine (RoutineBody exp) exp
type Routine = RoutineWithBody Expr

data EmptyBody = EmptyBody
               | EmptyExternal Text (Maybe Text)
                 deriving (Show, Eq, Ord, G.Generic, D.Data, T.Typeable)

data Contract exp = 
  Contract { contractInherited :: Bool 
           , contractClauses :: [Clause exp]
           } deriving (Show, Eq, Ord, G.Generic, D.Data, T.Typeable)

data AbsRoutine body exp = 
    AbsRoutine 
    { routineName   :: !Text
    , routineAlias  :: Maybe Text
    , routineArgs   :: [Decl]
    , routineResult :: Typ
    , routineAssigner :: Maybe Text
    , routineReq    :: Contract exp
    , routineImpl   :: !body
    , routineEns    :: Contract exp
    , routineRescue :: Maybe [PosAbsStmt exp]
    } deriving (Show, Eq, Ord, G.Generic, D.Data, T.Typeable)

data RoutineBody exp
  = RoutineExternal Text (Maybe Text)
  | RoutineBody 
    { routineLocal :: [Decl]
    , routineLocalProcs :: [ProcDecl]
    , routineBody  :: PosAbsStmt exp
    } deriving (Show, Eq, Ord, G.Generic, D.Data, T.Typeable)

data Attribute = 
  Attribute { attrDecl :: Decl
            , attrAssign :: Maybe Text
            } deriving (Show, Eq, Ord, G.Generic, D.Data, T.Typeable)
  
data Constant exp = 
  Constant { constDecl :: Decl
           , constVal :: exp
           } deriving (Show, Eq, Ord, G.Generic, D.Data, T.Typeable)

type Expr = Pos UnPosExpr 

data BinOp = Add
           | Sub
           | Mul
           | Div
           | Quot
           | Rem
           | Pow
           | Or
           | OrElse
           | Xor
           | And
           | AndThen
           | Implies
           | RelOp ROp Typ
           | SymbolOp Text
             deriving (Show, Ord, Eq, G.Generic, D.Data, T.Typeable)

data ROp = Lte
         | Lt 
         | Eq 
         | Neq
         | Gt 
         | Gte
           deriving (Show, Ord, Eq, G.Generic, D.Data, T.Typeable)

data UnOp = Not
          | Neg
          | Old
            deriving (Show, Ord, Eq, G.Generic, D.Data, T.Typeable)

data UnPosExpr =
    UnqualCall Text [Expr]
  | QualCall Expr Text [Expr]
  | Lookup Expr [Expr]
  | PrecursorCall (Maybe Text) [Expr]
  | BinOpExpr BinOp Expr Expr
  | UnOpExpr UnOp Expr
  | Address Expr
  | Attached (Maybe Typ) Expr (Maybe Text)
  | AcrossExpr Expr Text Quant Expr
  | Agent Expr
  | CreateExpr Typ Text [Expr]
  | InlineAgent [Decl] (Maybe Typ) [Stmt] [Expr]
  | TypedVar Text Typ
  | VarOrCall Text
  | ResultVar
  | CurrentVar
  | StaticCall Typ Text [Expr]
  | LitArray [Expr]
  | LitString Text
  | LitChar Char
  | LitInt Integer
  | LitBool Bool
  | LitVoid
  | LitDouble Double 
    deriving (Ord, Eq, G.Generic, D.Data, T.Typeable)

data Quant = All | Some deriving (Eq, Ord, Show, G.Generic, D.Data, T.Typeable)

commaSepShow es = intercalate "," (map show es)
argsShow args = "(" ++ commaSepShow args ++ ")"

defaultCreate :: Text
defaultCreate = "default_create"

instance Show UnPosExpr where
    show (UnqualCall s args) = "(unqual)" ++ show s ++ argsShow args
    show (QualCall t s args) = show t ++ "." ++ show s ++ argsShow args
    show (Lookup t args) = show t ++ "[" ++ commaSepShow args ++ "]"
    show (PrecursorCall t args) = "Precursor " ++ show t ++  argsShow args
    show (BinOpExpr op e1 e2) 
        = "(" ++ show e1 ++ " " ++ show op ++ " " ++ show e2 ++ ")"
    show (UnOpExpr op e) = "(" ++ show op ++ " " ++ show e ++ ")"
    show (Attached s1 e s2) = "(attached " ++ show s1 ++ ", " 
                              ++ show e ++ " as " ++ show s2 ++ ")"
    show (CreateExpr t s args)
        = "create {" ++ show t ++ "}." ++ show s 
          ++ "(" ++ intercalate "," (map show args) ++ ")"
    show (AcrossExpr c as quant e) = 
      "across " ++ show c ++ " as " ++ show as ++ " " 
      ++ show quant ++ " " ++ show e
    show (TypedVar var t) = "(" ++ show var ++ ": " ++ show t ++ ")"
    show (StaticCall t i args) = "{" ++ show t ++ "}." 
                                 ++ show i ++ argsShow args
    show (Address e) = "$" ++ show e
    show (VarOrCall s) = show s
    show ResultVar  = "Result"
    show CurrentVar = "Current"
    show (LitString s) = "\"" ++ show s ++ "\""
    show (LitChar c) = "'" ++ [c] ++ "'"
    show (LitInt i)  = show i
    show (LitBool b) = show b
    show (LitDouble d) = show d
    show (LitArray es) = "<<" ++ commaSepShow es ++ ">>"
    show (Agent e)  = "agent " ++ show e
    show (InlineAgent ds r ss args) = 
      "agent " ++ show ds ++ ":" ++ show r ++ " " ++ show ss 
      ++ " " ++ show args
    show LitVoid = "Void"

data Typ = ClassType ClassName [Typ]
         | AnyIntType
         | Int8Type
         | Int16Type
         | Int32Type
         | Int64Type
         | BoolType
         | DoubleType
         | CharType
         | ProcessorType
         | Sep (Maybe Proc) [Proc] Text
         | VoidType
         | NoType deriving (Eq, Ord, G.Generic, D.Data, T.Typeable)

instance Hashable Typ where
  hashWithSalt salt t =
    case t of
      NoType -> 0
      BoolType -> 1
      DoubleType -> 2
      CharType -> 3
      VoidType -> 4
      AnyIntType -> 5
      Int8Type -> 6
      Int16Type -> 7
      Int32Type -> 8
      Int64Type -> 9
      ProcessorType -> 11
      Sep _ _ name -> hashWithSalt salt name
      ClassType name _ -> hashWithSalt salt name

data Decl = Decl 
    { declName :: Text,
      declType :: Typ
    } deriving (Ord, Eq, G.Generic, D.Data, T.Typeable)
instance Hashable Decl

instance Show Decl where
    show (Decl name typ) = show name ++ ":" ++ show typ


data Proc = Dot 
          | Proc {unProcGen :: Text} 
            deriving (Eq, Ord, G.Generic, D.Data, T.Typeable)
instance Hashable Proc

instance Show Proc where
    show Dot = "<.>"
    show p = show $ unProcGen p

instance Show Typ where
    show (Sep c ps t)  = concat [ "separate <", show c, ">"
                                , show (map unProcGen ps)," ",show t
                                ]
    show NoType        = "notype"
    show VoidType      = "NONE"
    show AnyIntType    = "Integer"
    show Int8Type      = "Integer_8"
    show Int16Type     = "Integer_16"
    show Int32Type     = "Integer_32"
    show Int64Type     = "Integer_64"
    show ProcessorType = "<Processor>"
    show CharType      = "Character_8"
    show DoubleType    = "Real_64"
    show BoolType      = "Boolean"
    show (ClassType s gs) = show s ++ show gs

type ClassName = Text

type Stmt = PosAbsStmt Expr
type UnPosStmt = AbsStmt Expr
type PosAbsStmt a = Pos (AbsStmt a)
data AbsStmt a = Assign a a
               | If a (PosAbsStmt a) [ElseIfPart a] (Maybe (PosAbsStmt a))
               | Create (Maybe Typ) a Text [a]
               | Across a Text (PosAbsStmt a)
               | Loop (PosAbsStmt a) [Clause a] a (PosAbsStmt a) (Maybe a) 
               | CallStmt a
               | Separate [a] (PosAbsStmt a)
               | Retry
               | Inspect a [([a], PosAbsStmt a)] (Maybe (PosAbsStmt a))
               | Check [Clause a]
               | CheckBlock [Clause a] (PosAbsStmt a)
               | Block [PosAbsStmt a]
               | Debug Text (PosAbsStmt a)
               | BuiltIn deriving (Ord, Eq, G.Generic, D.Data, T.Typeable)

data ElseIfPart a = ElseIfPart a (PosAbsStmt a)
                    deriving (Show, Ord, Eq, G.Generic, D.Data, T.Typeable)

instance Show a => Show (AbsStmt a) where
    show (Block ss) = intercalate ";\n" . map show $ ss
    show (If b body elseifs elseMb) = concat
        [ "if ", show b, "\n"
        , "then ", show body, "\n"
        , "elseifs: ", show elseifs, "\n"
        , "else: ", show elseMb
        ]
    show (Inspect i cases def) = "inspect " ++ show i 
        ++ concat (map showCase cases)
        ++ showDefault def
    show (Across e as stmt) = "across " ++ show e ++ " " ++ show as ++ 
                              "\nloop\n" ++ show stmt ++ "\nend"
    show Retry = "retry"
    show (Check cs) = "check " ++ show cs ++ " end"
    show (CheckBlock e body) = "checkBlock " ++ show e ++ "\n" ++ show body
    show (Separate names body) = "separate: " ++ show names ++ "\n" ++ show body
    show (Create t trg fName args) = 
        concat ["create ", braced t, show trg, ".", show fName, show args]
    show (CallStmt e) = show e
    show (Assign i e) = show i ++ " := " ++ show e ++ "\n"
    show (Loop fr _ un l var) = "from" ++ show fr ++ " until" ++ show un ++
                          " loop " ++ show l ++ "variant" ++ show var ++ "end"
    show (Debug str stmt) = "debug (" ++ show str ++ ")\n" 
                            ++ show stmt ++ "end\n"
    show BuiltIn = "built_in"
  
braced t = case t of
  Nothing -> ""
  Just t' -> "{" ++ show t' ++ "}"
  
showCase (l, s) = "when " ++ show l ++ " then\n" ++ show s
showDefault Nothing = ""
showDefault (Just s) = "else\n" ++ show s

data ProcExpr = LessThan Proc Proc deriving (Show, Eq, Ord, G.Generic, D.Data, T.Typeable)

data ProcDecl = SubTop Proc
              | CreateLessThan Proc Proc 
                deriving (Show, Eq, Ord, G.Generic, D.Data, T.Typeable)

data Clause a = Clause 
    { clauseName :: Maybe Text
    , clauseExpr :: a
    } deriving (Show, Ord, Eq, G.Generic, D.Data, T.Typeable)

instance Binary Text where
  put = put . Text.encodeUtf8
  get = fmap Text.decodeUtf8 get

$( derive makeBinary ''Import )
$( derive makeBinary ''Typ )
$( derive makeBinary ''UnPosExpr )
$( derive makeBinary ''BinOp )
$( derive makeBinary ''Quant )
$( derive makeBinary ''Decl )
$( derive makeBinary ''UnOp )
$( derive makeBinary ''ROp )

$( derive makeBinary ''AbsStmt )
$( derive makeBinary ''ElseIfPart )

$( derive makeBinary ''ProcExpr )

$( derive makeBinary ''Constant )
$( derive makeBinary ''Attribute )
$( derive makeBinary ''AbsRoutine )
$( derive makeBinary ''EmptyBody )

$( derive makeBinary ''Contract )

$( derive makeBinary ''Proc )
$( derive makeBinary ''ProcDecl )
$( derive makeBinary ''Generic )
$( derive makeBinary ''Clause )
$( derive makeBinary ''CreateClause )
$( derive makeBinary ''AbsClas )

$( derive makeNFData ''Import )
$( derive makeNFData ''Typ )
$( derive makeNFData ''UnPosExpr )
$( derive makeNFData ''BinOp )
$( derive makeNFData ''Quant )
$( derive makeNFData ''Decl )
$( derive makeNFData ''UnOp )
$( derive makeNFData ''ROp )

$( derive makeNFData ''AbsStmt )
$( derive makeNFData ''ElseIfPart )

$( derive makeNFData ''ProcExpr )

$( derive makeNFData ''Constant )
$( derive makeNFData ''Attribute )
$( derive makeNFData ''AbsRoutine )
$( derive makeNFData ''RoutineBody )
$( derive makeNFData ''EmptyBody )

$( derive makeNFData ''Contract )

$( derive makeNFData ''Proc )
$( derive makeNFData ''ProcDecl )
$( derive makeNFData ''Generic )
$( derive makeNFData ''Clause )
$( derive makeNFData ''CreateClause )
$( derive makeNFData ''AbsClas )

makeLenses ''AbsClas
