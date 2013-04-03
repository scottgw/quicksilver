{-# LANGUAGE OverloadedStrings #-}
module Language.QuickSilver.Parser.Typ where

import           Control.Applicative ((<$>))

import           Data.Text (Text)

import           Language.QuickSilver.Syntax
import           Language.QuickSilver.Parser.Lex

import           Text.Parsec

classTyp :: Parser Typ
classTyp = do
  i  <- identifier
  let i' = case i of
        "INTEGER" -> "INTEGER_32"
        "CHARACTER" -> "CHARACTER_8"
        "REAL" -> "REAL_32"
        "STRING" -> "STRING_8"
        x -> x
  gs <- option [] (squares (typ `sepBy1` comma))
  return (ClassType i' gs)

tupleTyp :: Parser Typ
tupleTyp = do
  identifierNamed "TUPLE"
  let typeDeclP =
        Right <$> concat <$> try (decl `sepBy1` semicolon) <|>
        Left <$> (typ `sepBy1` comma)
  typeOrDecls <- option (Left []) (squares typeDeclP)
  return (TupleType typeOrDecls)

detTyp :: Parser Typ
detTyp = keyword TokDetachable >> (sepTyp <|> baseTyp)

attTyp :: Parser Typ
attTyp = keyword TokAttached >> baseTyp

typ :: Parser Typ
typ = detTyp <|> attTyp <|> sepTyp <|> baseTyp

baseTyp :: Parser Typ
baseTyp = tupleTyp <|> classTyp

sepTyp :: Parser Typ
sepTyp = do
  keyword TokSeparate
  p   <- return Nothing -- optionMaybe (angles procGen)
  ps  <- return [] -- option [] procGens
  cn  <- identifier
  return $ Sep p ps cn

decl :: Parser [Decl]
decl = do
  names <- identifier `sepBy1` comma <?> "Declaration identifier"
  decl' names

decl' :: [Text] -> Parser [Decl]
decl' varNames = do
  colon           <?> "Declaration ':'"
  typeName <- typ <?> "Declaration type"
  return $ map (flip Decl typeName) varNames

argumentList :: Parser [Decl]
argumentList = 
  option [] (concat `fmap` parens (decl `sepBy` optional semicolon))
