{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.QuickSilver.Parser.Class (clas, absClas, clasInterfaceP) where

import           Control.Applicative ((<$>), (<*), (*>))

import           Data.Generics

import           Language.QuickSilver.Syntax
import           Language.QuickSilver.Util
import           Language.QuickSilver.Parser.Lex
import           Language.QuickSilver.Parser.Clause
import           Language.QuickSilver.Parser.Expr
import           Language.QuickSilver.Parser.Feature
import           Language.QuickSilver.Parser.Typ

import           Text.Parsec

genericsP :: Parser [Typ]
genericsP = squares (sepBy genericP comma)

genericP :: Parser Typ
genericP = AnyRefType <$> identifier

invariants :: Parser [Clause Expr]
invariants = keyword TokInvariant >> many clause

create :: Parser CreateClause
create = do
  keyword TokCreate
  exports <- option [] (braces (identifier `sepBy` comma))
  names <- identifier `sepBy` comma
  return (CreateClause exports names)

importt =
  do keyword TokImport
     Import <$> identifier

absClas :: (Data body, Typeable body)
           => Parser body -> Parser (AbsClas body Expr)
absClas routineP = do
  impts <- many importt
  isMod <- (keyword TokClass >> return False) <|>
           (keyword TokModule >> return True)
  name <- identifier
  gen  <- option [] genericsP
  cs   <- many create
  (rts, attrs, cnsts)  <- classElems routineP
  invs <- option [] invariants
  keyword TokEnd
  return ( updGenerics gen $ AbsClas 
           { _imports    = impts
           , _className  = name
           , _generics   = gen 
           , _creates    = cs
           , _attributes = attrs
           , _routines   = rts
           , _consts     = cnsts
           , _invnts     = invs
           , _isModule   = isMod
           }
         )
  where
    updGenerics gs c = 
        foldr (\ t@(AnyRefType n) -> replaceType (ClassType n []) t) c gs

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
