{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.QuickSilver.Parser.Feature where

import           Control.Applicative ((<$>), (<*>))

import qualified Data.Text as Text
import           Data.Text (Text)

import           Language.QuickSilver.Syntax
import           Language.QuickSilver.Parser.Clause
import           Language.QuickSilver.Parser.Lex
import           Language.QuickSilver.Parser.Statement
import           Language.QuickSilver.Parser.Typ

import           Text.Parsec

type FeatParser body exp = 
    Parser body -> Parser (AbsRoutine body exp)

routine :: Text -> FeatParser body Expr
routine name implP  = do
  args  <- argumentList
  res   <- option NoType (colon >> typ)
  reqs  <- option (Contract True []) requires
  impl  <- implP
  ens   <- option (Contract True []) ensures
  rescue <- optionMaybe rescueP
  keyword TokEnd

  return $
    AbsRoutine
     { routineName = name
     , routineAlias  = Nothing
     , routineArgs   = args
     , routineResult = res
     , routineAssigner = Nothing
     , routineReq    = reqs
     , routineImpl   = impl
     , routineEns    = ens
     , routineRescue = rescue
     }

rescueP = do
  keyword TokRescue
  many stmt

assigner :: Parser Text
assigner = do
  keyword TokAssign
  identifier

allowedAliases :: [Text]
allowedAliases = ["[]", "|..|", "and", "and then", "or", "or else", "implies",
                  "xor", "not"]

alias = 
  let regStr = do  
        str <- stringTok
        if Text.all (\c -> Text.any (c ==) opSymbol) str || 
           str `elem` allowedAliases
          then return str
          else fail $ "unallowed alias symbol: " ++ Text.unpack str
      squareStr = do
        str <- stringTok -- FIXME: we don't lex block strings yet!, 
                         -- used to be: blockTextTok
        if str == "" then return "[]" else fail $ "unallowed alias symbol: [" ++ Text.unpack str ++ "]"
  in do
    keyword TokAlias
    regStr <|> squareStr

obsolete :: Parser Text
obsolete = keyword TokObsolete >> stringTok

whichOf :: Parser a -> Parser a -> Parser Bool
whichOf p1 p2 = (p1 >> return True) <|> (p2 >> return False)

requires :: Parser (Contract Expr)
requires = do 
  inherited <- whichOf (keyword TokRequireElse) (keyword TokRequire) 
  c <- many clause
  return $ Contract inherited c

ensures :: Parser (Contract Expr)
ensures = do 
  inherited <- whichOf (keyword TokEnsureThen) (keyword TokEnsure) 
  c <- many clause
  return $ Contract inherited c

external :: Parser (RoutineBody exp)
external = RoutineExternal <$> (keyword TokExternal >> anyStringTok)
                           <*> optionMaybe (keyword TokAlias >> anyStringTok)

routineImplP :: Parser (RoutineBody Expr)
routineImplP = do
  decls <- concat `fmap` option [] (keyword TokLocal >> many decl)
  external <|> (do body <- featBody
                   return (RoutineBody
                             { routineLocal = decls
                             , routineBody  = body
                             , routineLocalProcs = []
                             }
                             ))

featBody :: Parser Stmt 
featBody = attachTokenPos $
           (keyword TokDo <|> keyword TokOnce) >> 
           Block `fmap` stmts
