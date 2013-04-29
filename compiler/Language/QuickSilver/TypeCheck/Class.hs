module Language.QuickSilver.TypeCheck.Class 
       (clas, clasM, typeInterfaces, typedPre, runTyping) where

import           Control.Applicative
import           Control.Lens
import           Control.Monad.Reader

import qualified Data.HashMap.Strict as Map
import qualified Data.Traversable as Trav
import qualified Data.Text as Text
import           Data.Text (Text)

import           Language.QuickSilver.Syntax
import           Language.QuickSilver.Position
import           Language.QuickSilver.Util

import qualified Language.QuickSilver.TypeCheck.TypedExpr as T
import           Language.QuickSilver.TypeCheck.TypedExpr (TStmt, TClass)
import           Language.QuickSilver.TypeCheck.Context
import           Language.QuickSilver.TypeCheck.Expr

routineStmt :: RoutineBody Expr -> TypingBody body TStmt
routineStmt = stmt . routineBody

routineEnv :: AbsRoutine body Expr
              -> TypingBody ctxBody a
              -> TypingBody ctxBody a
routineEnv f m = local (addDecls (routineArgs f) . setResult f) m
 
runTyping :: [AbsClas ctxBody expr']
             -> AbsClas body expr
             -> TypingBodyExpr ctxBody expr' a
             -> Either String a
runTyping cs curr m = idErrorRead m (mkCtx (cType curr) cs)

clasM :: [AbsClas ctxBody Expr] 
         -> AbsClas (RoutineBody Expr) Expr 
         -> Either String TClass
clasM cs c = runTyping cs c (clas c routineWithBody)

clas :: AbsClas t Expr 
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
        check lens f = mapMOf traverse f (view lens c)

typeInterfaces :: [ClasInterface] -> 
                  IO (Either String [AbsClas EmptyBody T.TExpr])
typeInterfaces inters = 
  let 
    go :: ClasInterface -> IO (AbsClas EmptyBody T.TExpr)
    go i = do print (view className i)
              case runTyping inters i (interface i) of
                Left s -> error s
                Right i' -> return i'
  in do inters' <- mapM go inters
        return (Right inters')
     

interface :: AbsClas EmptyBody Expr 
             -> TypingBody ctxBoxy (AbsClas EmptyBody T.TExpr)
interface curr = clas curr return

cType :: AbsClas body exp -> Typ
cType c =
  ClassType (view className c) 
            (map (\ g -> ClassType (genericName g) []) (view generics c))

typedPre :: [ClasInterface] -> ClasInterface 
            -> Text -> Either String (Contract T.TExpr)
typedPre cis classInt name = idErrorRead go (mkCtx (cType classInt) cis)
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

contract :: Contract Expr -> TypingBody body (Contract T.TExpr)
contract (Contract inher cs) = Contract inher `fmap` mapM clause cs

constt :: Constant Expr -> TypingBody body (Constant T.TExpr)
constt (Constant d e) = Constant d `fmap` typeOfExpr e
-- TODO: Match the type of the expression with the 
-- delcared type of the constant.

routine :: (body -> TypingBody ctxBody body')
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

rescue :: Maybe [Stmt] -> TypingBody ctxBody (Maybe [TStmt])
rescue Nothing = return Nothing
rescue (Just ss) = Just <$> mapM stmt ss

routineWithBody :: RoutineBody Expr -> TypingBody body (RoutineBody T.TExpr)
routineWithBody (RoutineExternal var varMb) = return $ RoutineExternal var varMb
routineWithBody body = do
  statement <- local (addDecls (routineLocal body)) (routineStmt body)
  return (body {routineBody = statement})

stmt :: Stmt -> TypingBody body TStmt
stmt s = setPosition (position s) (uStmt (contents s))

uStmt :: UnPosStmt -> TypingBody body TStmt

uStmt (CallStmt e) = do
  e' <- typeOfExpr e
  tagPos (CallStmt e')
  
uStmt (Assign s e) = do
  s' <- typeOfExpr s
  e' <- typeOfExprIs (T.texpr s') e
  return $ inheritPos (Assign s') e'

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

uStmt (Loop setup invs cond body var) = do
  setup' <- stmt setup
  invs' <- mapM clause invs
  cond' <- typeOfExprIs boolType cond
  body' <- stmt body
  var' <- maybe (return Nothing) (fmap Just . typeOfExprIs intType) var
  tagPos (Loop setup' invs' cond' body' var')

uStmt (Block ss) = Block `fmap` mapM stmt ss >>= tagPos

uStmt (Print e)  = do
  e' <- typeOfExprIs intType e
  tagPos (Print e')

uStmt (PrintD e)  = do
  e' <- typeOfExprIs realType e
  tagPos (PrintD e')

uStmt (Create typeMb vr fName args) = do
  call <- tagPos (QualCall vr fName args) >>= typeOfExpr
  let call' = case contents call of
                T.Cast _ c -> c
                T.Call {} -> call
                _ -> error "uStmt: create only on casts or calls"
  let T.Call trg _ tArgs res = contents call'
  let ClassType _ _ = T.texpr trg
  guardThrow (res == NoType) 
                 "There must be no return type for create"
  guardThrow (maybe True (T.texpr trg ==) typeMb) -- FIXME: This should be inherits
                 "Target type doesn't match dynamic type"
  tagPos (Create typeMb trg fName tArgs)

uStmt BuiltIn = tagPos BuiltIn

-- uStmt s = error $ "uStmt: not implemented " ++ show s