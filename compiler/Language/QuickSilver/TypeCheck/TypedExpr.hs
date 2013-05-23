{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.QuickSilver.TypeCheck.TypedExpr where

import           Control.Applicative

import qualified Data.Data as D
import           Data.DeriveTH
import           Data.Binary
import           Data.Hashable
import           Data.Text (Text)
import qualified Data.Typeable as T

import qualified GHC.Generics as G

import qualified Language.QuickSilver.Syntax as E 
  (UnPosExpr (..), ROp (..), UnOp (..))
import           Language.QuickSilver.Syntax hiding ( UnPosExpr (..)
                                                    , ROp (..)
                                                    , UnOp (..)
                                                    )
import           Language.QuickSilver.Position
import           Language.QuickSilver.Util

type TClass = ClasBody TExpr
type TRoutine = RoutineWithBody TExpr
type TStmt = PosAbsStmt TExpr
type UnPosTStmt = AbsStmt TExpr

type TExpr = Pos UnPosTExpr


data EqOp = Eq | Neq
            deriving (Show, Eq, G.Generic, D.Data, T.Typeable)

instance Hashable EqOp

eqOp (RelOp E.Eq _) = Eq
eqOp (RelOp E.Neq _) = Neq
eqOp r = error $ "eqOp: " ++ show r

binEqOp Eq = RelOp E.Eq NoType
binEqOp Neq = RelOp E.Neq NoType

data UnPosTExpr 
  = Call TExpr Text [TExpr] Typ
  | BinOpExpr BinOp TExpr TExpr Typ
  | UnOpExpr E.UnOp TExpr Typ
  | Access TExpr Text Typ
  | Agent TExpr Text [TExpr] Typ
  | InheritProc TExpr TExpr
  | Old TExpr
  | Var Text Typ
  | EqExpr EqOp TExpr TExpr
  | CreateExpr Typ Text [TExpr]
  | Attached (Maybe Typ) TExpr (Maybe Text)
  | StaticCall Typ Text [TExpr] Typ
  | ResultVar Typ
  | CurrentVar Typ
  | CurrentProc
  | Box Typ TExpr
  | Unbox Typ TExpr
  | Cast Typ TExpr
  | LitArray [TExpr]
  | LitChar Char
  | LitString Text
  | LitInt Integer Typ
  | LitBool Bool
  | LitVoid Typ
  | LitDouble Double 
    deriving (Show, Eq, G.Generic, D.Data, T.Typeable)

instance Hashable UnPosTExpr

$( derive makeBinary ''EqOp )
$( derive makeBinary ''UnPosTExpr )

untype :: TClass -> Clas
untype = classMapExprs untypeRout untypeClause untypeConstant

untypeClause :: Clause TExpr -> Clause Expr
untypeClause (Clause label e) = Clause label (untypeExpr e)

untypeContract (Contract inhrt clauses) = 
  Contract inhrt (map untypeClause clauses)

untypeConstant (Constant decl expr) = 
  Constant decl (untypeExpr expr)

untypeRout :: TRoutine -> Routine
untypeRout tfeat = 
  tfeat { routineImpl = untypeImpl (routineImpl tfeat)
        , routineReq  = untypeContract (routineReq tfeat)
        , routineEns  = untypeContract (routineEns tfeat)
        , routineRescue = map untypeStmt `fmap` routineRescue tfeat
        }

untypeImpl :: RoutineBody TExpr -> RoutineBody Expr
untypeImpl body = body {routineBody = untypeStmt (routineBody body)}

untypeStmt :: TStmt -> Stmt
untypeStmt = fmap untypeStmt'

untypeStmt' :: UnPosTStmt -> UnPosStmt
untypeStmt' (Assign l e) = Assign (untypeExpr l) (untypeExpr e)
untypeStmt' (CallStmt e) = CallStmt (untypeExpr e)
untypeStmt' (Block ss)   = Block (map untypeStmt ss)
untypeStmt' BuiltIn      = BuiltIn
untypeStmt' (Check cs)   = Check (map untypeClause cs)
untypeStmt' (Loop from inv untl body var) =
  Loop (untypeStmt from)
       (map untypeClause inv)
       (untypeExpr untl)
       (untypeStmt body)
       (untypeExpr <$> var)
untypeStmt' (If e body elseIfs elsePart) = 
  let untypeElseIf (ElseIfPart cond s) = 
        ElseIfPart (untypeExpr cond) (untypeStmt s)
  in If (untypeExpr e) (untypeStmt body)
        (map untypeElseIf elseIfs) (fmap untypeStmt elsePart)
untypeStmt' (Create typeMb targ name es) = 
  Create typeMb (untypeExpr targ) name (map untypeExpr es)
untypeStmt' s = error $ "untypeStmt': " ++ show s



untypeExpr :: TExpr -> Expr
untypeExpr = fmap untypeExpr'

untypeExpr' :: UnPosTExpr -> E.UnPosExpr
untypeExpr' (Call trg name args _r)
    = E.QualCall (untypeExpr trg) name (map untypeExpr args)
untypeExpr' (Access trg name _r)
    = E.QualCall (untypeExpr trg) name []
untypeExpr' (Var s _t)
    = E.VarOrCall s
untypeExpr' (CurrentVar _t)
    = E.CurrentVar
untypeExpr' (Old e) = E.UnOpExpr E.Old (untypeExpr e)
untypeExpr' (Cast _ e) 
    = contents $ untypeExpr e
untypeExpr' (ResultVar _t)
    = E.ResultVar
untypeExpr' (EqExpr op e1 e2)
  = E.BinOpExpr (binEqOp op) (untypeExpr e1) (untypeExpr e2)
untypeExpr' (BinOpExpr op e1 e2 _)
  = E.BinOpExpr op (untypeExpr e1) (untypeExpr e2)
untypeExpr' (UnOpExpr op e _)
  = E.UnOpExpr op (untypeExpr e)
untypeExpr' (Attached typ e asName)
  = E.Attached typ (untypeExpr e) asName
untypeExpr' (Box _ e) = contents $ untypeExpr e
untypeExpr' (Unbox _ e) = contents $ untypeExpr  e
untypeExpr' (LitArray es) = E.LitArray (map untypeExpr es)
untypeExpr' (LitChar c) = E.LitChar c
untypeExpr' (LitString s) = E.LitString s
untypeExpr' (LitInt i _t) = E.LitInt i
untypeExpr' (LitBool b) =  E.LitBool b
untypeExpr' (LitVoid _) = E.LitVoid
untypeExpr' (LitDouble d) = E.LitDouble d
untypeExpr' (Agent trg name args _) 
  = E.Agent $ 
    takePos trg $ 
    E.QualCall (untypeExpr trg) name (map untypeExpr args)
untypeExpr' (CreateExpr t fname args) = 
  E.CreateExpr t fname $ map untypeExpr args
untypeExpr' s = error $ "untypeExpr': " ++ show s      

texpr :: TExpr -> Typ
texpr = texprTyp . contents

texprTyp :: UnPosTExpr -> Typ
texprTyp (CreateExpr t _ _) = t
texprTyp (BinOpExpr _ _ _ t) = t
texprTyp (UnOpExpr _ _ t) = t
texprTyp (Agent _ _ _ t) = t
texprTyp (Var _ t)   = t
texprTyp (Cast t _)  = t
texprTyp (ResultVar t) = t
texprTyp (CurrentVar t) = t
texprTyp (InheritProc _inh base) = Sep Nothing [] (texpr base)
texprTyp CurrentProc = ProcessorType
texprTyp (Call _ _ _ t) = t
texprTyp (Access _ _ t) = t
texprTyp (EqExpr{}) = boolType
texprTyp (Box _ te) = texpr te
texprTyp (Unbox t _) = t
texprTyp (Old e) = texprTyp (contents e)
texprTyp (StaticCall _ _ _ t) = t
texprTyp (LitChar _) = charType
texprTyp (Attached{}) = boolType
texprTyp (LitString _) = stringType
texprTyp (LitInt _ t)  = t
texprTyp (LitBool _) = boolType
texprTyp (LitDouble _) = realType
texprTyp (LitVoid  t) = t
