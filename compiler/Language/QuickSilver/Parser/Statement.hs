{-# LANGUAGE ScopedTypeVariables #-}

module Language.QuickSilver.Parser.Statement where

import qualified Data.Text as Text

import           Language.QuickSilver.Syntax
import           Language.QuickSilver.Parser.Clause
import           Language.QuickSilver.Parser.Expr
import           Language.QuickSilver.Parser.Lex
import           Language.QuickSilver.Parser.Typ

import           Text.Parsec

-- stmt :: Parser Stmt
stmt = attachTokenPos bareStmt

-- bareStmt :: Parser UnPosStmt
bareStmt = do
     s <- choice [ across
                 , assign
                 , check
                 , retry
                 , create
                 , separate
                 , passive
                 , ifStmt
                 , inspect
                 , loop
                 , debug
                 , shutdown
                 , try callStmt
                 ]
     optional semicolon
     return s

stmts :: Parser [Stmt]
stmts = many stmt

stmts' = many bareStmt

shutdown =
  do keyword TokShutdown
     e <- expr
     return (Shutdown e)

separate =
  do keyword TokSeparate
     args <- many expr
     clauses <- option [] (keyword TokRequire >> many clause)
     keyword TokDo
     ss <- attachTokenPos block
     keyword TokEnd
     return (Separate args clauses ss)

passive =
  do keyword TokPassive
     args <- many expr
     keyword TokDo
     ss <- attachTokenPos block
     keyword TokEnd
     return (Passive args ss)
                   

retry = do
  keyword TokRetry
  return Retry

across = do
  keyword TokAcross
  e <- expr
  keyword TokAs
  i <- identifier
  keyword TokLoop
  bl <- blockPos
  keyword TokEnd
  return (Across e i bl)

inspect = 
  let whenPart = do 
        keyword TokWhen
        es <- expr `sepBy1` comma
        s <- attachTokenPos (keyword TokThen >> Block `fmap` stmts)
        return (es, s)
  in do
    keyword TokInspect
    e <- expr
    whens  <- many1 whenPart
    elseMb <- optionMaybe (attachTokenPos $ keyword TokElse >> Block `fmap` stmts)
    keyword TokEnd
    return $ Inspect e whens elseMb

check = do
  keyword TokCheck
  clauses <- many clause
  let chk = keyword TokEnd >> return (Check clauses)
      checkBlock = do
        keyword TokThen
        body <- blockPos
        keyword TokEnd
        return (CheckBlock clauses body)
  checkBlock <|> chk


blockPos = attachTokenPos block

block :: Parser UnPosStmt
block = fmap Block stmts

ifStmt :: Parser UnPosStmt
ifStmt = do
  b  <- keyword TokIf >> expr
  body <- attachTokenPos (keyword TokThen >> fmap Block stmts)
  ifelses <- many ifelseP
  elseMb <- optionMaybe elseP
  elseMb' <- maybe (return Nothing) (fmap Just . attachTokenPos . return) elseMb
  keyword TokEnd
  return (If b body ifelses elseMb')

-- elsePart :: Parser UnPosStmt
-- elsePart = ifelseP <|> elseP

elseP :: Parser UnPosStmt
elseP = keyword TokElse >> fmap Block stmts

ifelseP :: Parser (ElseIfPart Expr)
ifelseP = do
  b <- keyword TokElseIf >> expr
  s1 <- attachTokenPos $ keyword TokThen >> fmap Block stmts
  -- s2 <- attachTokenPos $ option (Block []) elsePart
  return (ElseIfPart b s1)

create :: Parser UnPosStmt
create = do
  keyword TokCreate
  t <- optionMaybe (braces typ)
  v <- attachTokenPos var
  s <- (do
         period
         callE <- call
         case callE of
           UnqualCall fName args -> return (Create t v fName args)
           VarOrCall fName -> return (Create t v fName [])
           e -> error $ "create: should not have parsed " ++ show e
       ) <|> return (Create t v defaultCreate [])
  return s

loop :: Parser UnPosStmt
loop = do
  keyword TokFrom
  fr <- attachTokenPos block
  invarMb <- option [] (keyword TokInvariant >> many clause)
  un <- keyword TokUntil >> expr
  lo <- attachTokenPos $ keyword TokLoop >> block
  variant <- optionMaybe (keyword TokVariant >> clauseExpr `fmap` clause)
  keyword TokEnd
  return (Loop fr invarMb un lo variant)

assignId :: Parser Expr
assignId = do
  e <- expr
  colon
  opInfo (RelOp Eq NoType)
  return e
  
assignAttemptId :: Parser Expr
assignAttemptId = do
  i <- attachTokenPos var
  symbol '?'
  opInfo (RelOp Eq NoType)
  return i  

callStmt :: Parser UnPosStmt
callStmt = do
  c <- attachTokenPos call
  return $ CallStmt c

assign :: Parser UnPosStmt
assign = do
  i <- try assignId
  e <- expr <?> "assignment expression"
  return $ Assign i e

debug :: Parser UnPosStmt
debug = do
  keyword TokDebug
  str <- option Text.empty (parens anyStringTok)
  b <- attachTokenPos block
  keyword TokEnd
  return (Debug str b)
