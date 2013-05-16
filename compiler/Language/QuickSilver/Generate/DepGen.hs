{-# LANGUAGE OverloadedStrings #-}
module Language.QuickSilver.Generate.DepGen (depGen, depGenInt) where

import Control.Lens
import Control.Monad
import Control.Monad.Error

import Data.Text (Text)

import Language.QuickSilver.Syntax
import Language.QuickSilver.Util
import Language.QuickSilver.Parser

import Text.Parsec.Error
import Text.Parsec.Pos

type DepM = ErrorT ParseError IO

instance Error ParseError where
    strMsg s = newErrorMessage (Message s) (initialPos "NoFile") 

depGen :: ClassName -> IO (Either ParseError [Clas])
depGen name = runErrorT $ depGen' name []

depGenInt :: ClassName -> IO (Either ParseError [ClasInterface])
depGenInt = fmap (fmap (map clasInterface)) . depGen

depGen' :: ClassName -> [Clas] -> DepM [Clas]
depGen' cn acc = do
  newClassEi <- lift (parseClassFile (classNameFile cn))
  case newClassEi of
    Left str -> throwError $ strMsg $ "depGen'->" ++ show str
    Right newClass -> depClas newClass acc

depClas :: Clas -> [Clas] -> DepM [Clas]
depClas c acc
  | acc `hasClas` view className c = return acc
  | otherwise = foldM go acc' imps
  where
    go :: [Clas] -> Import -> DepM [Clas]
    go cs (Import name) = depGen' name cs
    acc' = c:acc
    imps = view imports c

hasClas :: [Clas] -> ClassName -> Bool
hasClas classes cn = any ( (==) cn . view className) classes
