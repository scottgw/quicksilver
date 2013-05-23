{-# LANGUAGE OverloadedStrings #-}
module Language.QuickSilver.Parser.Typ where

import           Data.Text (Text)

import           Language.QuickSilver.Syntax
import           Language.QuickSilver.Parser.Lex

import           Text.Parsec

classTyp :: Parser Typ
classTyp = do
  i  <- identifier
  let i' = case i of
        "STRING" -> "STRING_8"
        x -> x
  gs <- option [] (squares (typ `sepBy1` comma))
  return (ClassType i' gs)

basicType :: Parser Typ
basicType = choice $ map tyFunc nameAndType
  where
    nameAndType :: [(Text, Typ)]
    nameAndType = 
      [ ("Character", CharType)
      , ("Character_8", CharType)
      , ("Integer", Int64Type)
      , ("Integer_8", Int8Type)
      , ("Integer_64", Int64Type)
      , ("Real", DoubleType)
      , ("Boolean", BoolType)
      ]

    tyFunc :: (Text, Typ) -> Parser Typ
    tyFunc (name, ty) = identifierNamed name >> return ty

detTyp :: Parser Typ
detTyp = keyword TokDetachable >> (sepTyp <|> baseTyp)

attTyp :: Parser Typ
attTyp = keyword TokAttached >> baseTyp

typ :: Parser Typ
typ = detTyp <|> attTyp <|> baseTyp <|> sepTyp

baseTyp :: Parser Typ
baseTyp = basicType <|> classTyp

sepTyp :: Parser Typ
sepTyp = do
  keyword TokSeparate
  p   <- return Nothing -- optionMaybe (angles procGen)
  ps  <- return [] -- option [] procGens
  t   <- typ
  return $ Sep p ps t

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
