{-# LANGUAGE OverloadedStrings #-}
module Language.QuickSilver.Generate.DepGen (depGen, depGenInt) where

import Control.Lens
import Control.Monad
import Control.Monad.Error

import qualified Data.Text as Text

import Language.QuickSilver.Syntax
import Language.QuickSilver.Util
import Language.QuickSilver.Parser

import Text.Parsec.Error
import Text.Parsec.Pos

import System.Directory
import System.Environment
import qualified System.FilePath.Find as FilePath

type DepM = ErrorT ParseError IO

instance Error ParseError where
    strMsg s = newErrorMessage (Message s) (initialPos "NoFile") 

searchPathEnvVar = "QS_LIB_PATH"

parseFromSearchPath name =
  lift $
    do searchPath <- getEnv searchPathEnvVar
       let
         file = classNameFile name
         searchSpec = FilePath.find
                          FilePath.always
                          (FilePath.fileName FilePath.==? file)
       filesPath <- searchSpec searchPath
       filesCurr <- getCurrentDirectory >>= searchSpec
       case filesPath ++ filesCurr of
         filePath:_ -> parseClassFile filePath
         _ -> error $ "parseFromSearchPath: not found " ++ Text.unpack name

depGen :: ClassName -> IO (Either ParseError [Clas])
depGen name = runErrorT $ depGen' name []

depGenInt :: ClassName -> IO (Either ParseError [ClasInterface])
depGenInt = fmap (fmap (map clasInterface)) . depGen

depGen' :: ClassName -> [Clas] -> DepM [Clas]
depGen' cn acc = do
  newClassEi <- parseFromSearchPath cn
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
