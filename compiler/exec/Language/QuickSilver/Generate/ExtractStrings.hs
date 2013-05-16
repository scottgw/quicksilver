module Language.QuickSilver.Generate.ExtractStrings where

import Control.Lens

import qualified Data.Text as Text

import Language.QuickSilver.Syntax 
    ( AbsStmt (..)
    , AbsRoutine (..)
    , RoutineBody (..)
    , Clause (..)
    , ElseIfPart (..)
    , contractClauses
    , routines
    )
import Language.QuickSilver.Util
import Language.QuickSilver.Position
import Language.QuickSilver.TypeCheck.TypedExpr

import Language.QuickSilver.Generate.LLVM.Simple

fromClass :: TClass -> Build ()
fromClass = mapM_ fromRoutine . view routines

fromRoutine :: TRoutine -> Build ()
fromRoutine f = 
  fromClauses (concatMap contractClauses [routineReq f, routineEns f]) >>
  fromBody (routineImpl f)

fromBody (RoutineBody _ _ body) = fromStmt body
fromBody _ = return ()

fromClause = fromExpr . clauseExpr
fromClauses = mapM_ fromClause

fromStmt = fromStmt' . contents

fromStmt' (Assign e1 e2) = fromExpr e1 >> fromExpr e2
fromStmt' (If e then_ elseIfs elseMb) = do
  fromExpr e
  fromStmt then_
  let fromElseIf (ElseIfPart cond stmt) = fromExpr cond >> fromStmt stmt
  mapM_ fromElseIf elseIfs
  maybe (return ()) fromStmt elseMb
fromStmt' (Malloc _) = return ()
fromStmt' (Create _typeMb e _trgVar es) = mapM_ fromExpr (e:es)
fromStmt' (Loop fr invs un loop varMb) = 
  fromStmt fr >> fromClauses invs >> fromExpr un >> fromStmt loop >>
  maybe (return ()) fromExpr varMb
fromStmt' (CallStmt e) = fromExpr e
fromStmt' (Block ss) = mapM_ fromStmt ss
fromStmt' BuiltIn = return ()
fromStmt' (Print _) = return ()
fromStmt' (PrintD _) = return ()

fromExpr = fromExpr' . contents

fromExpr' (LitString str) = string (Text.unpack str) >> return ()
fromExpr' (StaticCall _ _ args _) = mapM_ fromExpr args
fromExpr' (Call trg _ args _) = fromExpr trg >> mapM_ fromExpr args
fromExpr' (BinOpExpr _ e1 e2 _) = fromExpr e1 >> fromExpr e2
fromExpr' (Access trg _ _) = fromExpr trg
fromExpr' (Var _ _) = return ()
fromExpr' (Attached _ e _) = fromExpr e
fromExpr' (ResultVar _) = return ()
fromExpr' (CurrentVar _) = return ()
fromExpr' (Box _ e) = fromExpr e
fromExpr' (Unbox _ e) = fromExpr e
fromExpr' (Cast _ e) = fromExpr e
fromExpr' (LitChar _) = return ()
fromExpr' (LitInt _) = return ()
fromExpr' (LitBool _) = return ()
fromExpr' (LitVoid _) = return ()
fromExpr' (LitDouble _) = return ()
