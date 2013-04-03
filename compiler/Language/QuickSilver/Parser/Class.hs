{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.QuickSilver.Parser.Class (clas, absClas, clasInterfaceP) where

import           Control.Applicative ((<$>), (<*>), (<*), (*>))

import           Language.QuickSilver.Syntax

import           Language.QuickSilver.Parser.Lex
import           Language.QuickSilver.Parser.Clause
import           Language.QuickSilver.Parser.Expr
import           Language.QuickSilver.Parser.Feature
import           Language.QuickSilver.Parser.Typ

import           Text.Parsec

genericsP :: Parser [Generic]
genericsP = squares (sepBy genericP comma)

genericP :: Parser Generic
genericP = do
  name <- identifier
  typs <- option [] (do opNamed "->" 
                        braces (typ `sepBy1` comma) <|> fmap (replicate 1) typ)
  creations <- optionMaybe 
              (keyword TokCreate *> (identifier `sepBy1` comma) <* keyword TokEnd)
  return (Generic name typs creations)

invariants :: Parser [Clause Expr]
invariants = keyword TokInvariant >> many clause

create :: Parser CreateClause
create = do
  keyword TokCreate
  exports <- option [] (braces (identifier `sepBy` comma))
  names <- identifier `sepBy` comma
  return (CreateClause exports names)

absClas :: Parser body -> Parser (AbsClas body Expr)
absClas routineP = do
  keyword TokClass
  name <- identifier
  gen  <- option [] genericsP
  cs   <- many create
  (rts, attrs, cnsts)  <- classElems routineP
  invs <- option [] invariants
  keyword TokEnd
  return ( AbsClas 
           { _className  = name
           , _generics   = gen 
           , _creates    = cs
           , _attributes = attrs
           , _routines   = rts
           , _consts     = cnsts
           , _invnts     = invs
           }
         )


data ClassElem body expr =
  RoutElem (AbsRoutine body expr)
  | AttrElem Attribute
  | CnstElem (Constant expr)

classElems routP =
  do unsortedElems <- many (classElem routP)
     return (foldr f ([], [], []) unsortedElems)
  where
    f (RoutElem r) (rs, as, cs) = (r:rs, as, cs)
    f (AttrElem a) (rs, as, cs) = (rs, a:as, cs)
    f (CnstElem c) (rs, as, cs) = (rs, as, c:cs)

classElem routP =
  do name <- identifier
     constOrAttr name <|> (RoutElem <$> routine name routP)

constOrAttr name =
  do t <- colon *> typ
     let cnst = 
           do e <- opInfo (RelOp Eq NoType) >> expr
              optional semicolon
              return (CnstElem $ Constant (Decl name t) e)
         attr = return (AttrElem $ Attribute (Decl name t) Nothing)
     cnst <|> attr

clas :: Parser Clas
clas = absClas routineImplP

clasInterfaceP :: Parser ClasInterface
clasInterfaceP =  absClas (keyword TokDo >> return EmptyBody)
