{-# LANGUAGE BangPatterns #-}
module Language.QuickSilver.TypeCheck.Class 
       (clas, clasM, typeInterfaces, typedPre, runTyping) where

import           Control.Applicative
import           Control.Lens hiding (pre)
import           Control.Monad.Error
import           Control.Monad.Reader

import           Data.Generics
import qualified Data.HashMap.Strict as Map
import           Data.Text (Text)

import           Language.QuickSilver.Syntax
import           Language.QuickSilver.Position
import           Language.QuickSilver.Util

import qualified Language.QuickSilver.TypeCheck.TypedExpr as T
import           Language.QuickSilver.TypeCheck.TypedExpr (TStmt, TClass)
import           Language.QuickSilver.TypeCheck.Context
import           Language.QuickSilver.TypeCheck.Expr
import           Language.QuickSilver.TypeCheck.TypeError

routineStmt :: (Data body, Typeable body)
         => RoutineBody Expr -> TypingBody body TStmt
routineStmt = stmt . routineBody

routineEnv :: AbsRoutine body Expr
              -> TypingBody ctxBody a
              -> TypingBody ctxBody a
routineEnv f m = local (addDecls (routineArgs f) . setResult f) m

runTyping :: [AbsClas ctxBody expr']
             -> AbsClas body expr
             -> TypingBodyExpr ctxBody expr' a
             -> Either TypeError a
runTyping cs curr m =
  idErrorRead m (mkCtx (maybeCurrType curr) cs)

maybeCurrType cls
  | view isModule cls = Left (classToType cls)
  | otherwise = Right (classToType cls)

clasM :: (Data ctxBody, Typeable ctxBody)
         => [AbsClas ctxBody Expr] 
         -> AbsClas (RoutineBody Expr) Expr 
         -> Either TypeError TClass
clasM cs c = runTyping cs c (clas c routineWithBody)

clas :: (Data body, Typeable body, Data ctxBody, Typeable ctxBody)
        => AbsClas t Expr
        -> (t -> TypingBody ctxBody body) 
        -> TypingBody ctxBody (AbsClas body T.TExpr)
clas c rtnBodyCheck = 
  let gens = genericStubs c
      updateGen ctx = 
        ctx { interfaces = Map.union (interfaces ctx) (clasMap gens)}
  in local updateGen $ 
     do routs <- check routines (routine rtnBodyCheck)
        attrs <- check attributes attr
        cnsts <- check consts constt
        invs <- mapM clause (view invnts c)

        return $ c {_routines = routs
                   ,_attributes = attrs
                   ,_consts = cnsts
                   ,_invnts = invs
                   }
      where
        check lenss f = mapMOf traverse f (view lenss c)

typeInterfaces :: [ClasInterface] -> 
                  IO (Either TypeError [AbsClas EmptyBody T.TExpr])
typeInterfaces inters = 
  let 
    go :: ClasInterface -> IO (AbsClas EmptyBody T.TExpr)
    go i = do print (view className i)
              case runTyping inters i (interface i) of
                Left err -> error (show err)
                Right i' -> return i'
  in do inters' <- mapM go inters
        return (Right inters')
     

interface :: (Data ctxBody, Typeable ctxBody)
             => AbsClas EmptyBody Expr 
             -> TypingBody ctxBody (AbsClas EmptyBody T.TExpr)
interface curr = clas curr return

typedPre :: [ClasInterface] -> ClasInterface 
            -> Text -> Either TypeError (Contract T.TExpr)
typedPre cis classInt name = idErrorRead go (mkCtx (maybeCurrType classInt) cis)
  where Just rout = findAbsRoutine classInt name 
        go = routineEnv rout
                   (do r <- routine (const (return EmptyBody)) rout
                       return (routineReq r))


-- checkFeatureMap checkBody fm = do

-- someFeature :: (body -> TypingBody ctxBody body')  
--               -> SomeFeature body Expr 
--               -> TypingBody ctxBody (SomeFeature body' T.TExpr)
-- someFeature checkBody = checkSome
--   where
--     checkSome (SomeRoutine r) = SomeRoutine <$> routine checkBody r
--     checkSome (SomeAttr a) = SomeAttr <$> attr a
--     checkSome (SomeConst c) = SomeConst <$> constt c

attr :: Attribute -> TypingBody body Attribute
attr a = return a

contract :: (Data body, Typeable body)
         => Contract Expr -> TypingBody body (Contract T.TExpr)
contract (Contract inher cs) = Contract inher `fmap` mapM clause cs

constt :: (Data body, Typeable body)
         => Constant Expr -> TypingBody body (Constant T.TExpr)
constt (Constant d e) = Constant d `fmap` typeOfExpr e
-- TODO: Match the type of the expression with the 
-- delcared type of the constant.

routine :: (Data ctxBody, Typeable ctxBody)
           => (body -> TypingBody ctxBody body')
           -> AbsRoutine body Expr 
           -> TypingBody ctxBody (AbsRoutine body' T.TExpr)
routine checkBody f = 
    routineEnv f
      (do
          pre  <- contract (routineReq f)
          post <- contract (routineEns f)
          body <- checkBody (routineImpl f)
          resc <- rescue (routineRescue f)
          return $ f { routineReq = pre
                     , routineImpl = body
                     , routineEns = post
                     , routineRescue = resc
                     }
          )

rescue :: (Data ctxBody, Typeable ctxBody)
         => Maybe [Stmt] -> TypingBody ctxBody (Maybe [TStmt])
rescue Nothing = return Nothing
rescue (Just ss) = Just <$> mapM stmt ss

routineWithBody :: (Data body, Typeable body)
                   => RoutineBody Expr
                   -> TypingBody body (RoutineBody T.TExpr)
routineWithBody (RoutineExternal var varMb) = return $ RoutineExternal var varMb
routineWithBody body = do
  statement <- local (addDecls (routineLocal body)) (routineStmt body)
  return (body {routineBody = statement})

stmt :: (Data body, Typeable body)
        => Stmt -> TypingBody body TStmt
stmt s = setPosition (position s) (uStmt (contents s))

uStmt :: (Data body, Typeable body)
         => UnPosStmt -> TypingBody body TStmt

uStmt (CallStmt e) = do
  e' <- typeOfExpr e
  tagPos (CallStmt e')

uStmt (Assign var e) = do
  var'  <- typeOfExpr var
  e'  <- typeOfExpr e
  let eType = T.texpr e'
      varType = T.texpr var'
  e'' <- if (eType == AnyIntType && isIntegerType varType) ||
            (eType == VoidType && not (isBasic varType))
         then tagPos (T.Cast varType e') 
         else if T.texpr e' == T.texpr var' 
              then return e'
              else throwErrorPos (TypeMismatch varType eType)
  return $ inheritPos (Assign var') e''

uStmt (If cond body elseIfs elsePart) = do
  cond' <- typeOfExprIs boolType cond
  body' <- stmt body
  let checkElseIf (ElseIfPart c s) = do
        c' <- typeOfExprIs boolType c
        s' <- stmt s
        return (ElseIfPart c' s')
  elseIfs' <- mapM checkElseIf elseIfs
  else' <- case elsePart of
              Nothing -> return Nothing
              Just e  -> Just `fmap` stmt e
  tagPos (If cond' body' elseIfs' else')

uStmt (Shutdown e) =
  do e' <- typeOfExpr e
     case T.texpr e' of
       Sep _ _ _ -> tagPos (Shutdown e')
       _ -> throwErrorPos SeparateShutdown

uStmt (Separate args clauses body) =
  do args' <- mapM typeOfExpr args
     clauses' <- mapM clause clauses
     let varOrAccess e =
           case contents e of
             T.Var _ _ -> True
             T.Access _ _ _ -> True
             _ -> False
     mapM_ (\e -> guardThrow (varOrAccess e) SeparateBlockArg) args'
     body' <- local (addSeparates args') (stmt body)
     tagPos (Separate args' clauses' body')

uStmt (Passive args body) =
  do args' <- mapM typeOfExpr args
     hasAllArgs <- hasSeparates args'
     guardThrow hasAllArgs PassiveBlockReserve
     body' <- stmt body
     tagPos (Passive args' body')

uStmt (Loop setup invs cond body var) = do
  setup' <- stmt setup
  invs' <- mapM clause invs
  cond' <- typeOfExprIs boolType cond
  body' <- stmt body
  var' <- maybe (return Nothing) (fmap Just . typeOfExprIs intType) var
  tagPos (Loop setup' invs' cond' body' var')

uStmt (Block ss) = Block `fmap` mapM stmt ss >>= tagPos

uStmt (Create typeMb vr fName args) = do
  call <- tagPos (QualCall vr fName args) >>= typeOfExpr
  let call' = case contents call of
                T.Cast _ c -> c
                T.InheritProc _ c -> c
                T.Call {} -> call
                _ -> error $ "uStmt: create only on casts or calls " ++ show call
  let T.Call trg _ tArgs res = contents call'
  let ClassType _ _ = T.texpr trg
  guardThrow (res == NoType) CreateNoType
  case typeMb of
    Nothing -> return ()
    Just typ ->
      guardThrow (T.texpr trg == typ) -- FIXME: This should be inherits
                 (TypeMismatch (T.texpr trg) typ)
  tagPos (Create typeMb trg fName tArgs)

uStmt BuiltIn = tagPos BuiltIn

-- uStmt s = error $ "uStmt: not implemented " ++ show s
