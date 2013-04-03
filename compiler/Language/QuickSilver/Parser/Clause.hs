module Language.QuickSilver.Parser.Clause where

import Control.Applicative ((<$>), (<*))

import Language.QuickSilver.Syntax

import Language.QuickSilver.Parser.Expr
import Language.QuickSilver.Parser.Lex
import Language.QuickSilver.Position

import Text.Parsec

clause :: Parser (Clause Expr)
clause = do
  let tagNoExpr = do
        i <- identifier <* colon
        p <- getPosition
        e <- option (attachPos p (LitBool True)) expr
        return (Clause (Just i) e)
      withExpr = do
        tag <- try (Just <$> identifier <* colon) <|> return Nothing
        Clause tag <$> expr
    in try tagNoExpr <|> withExpr
