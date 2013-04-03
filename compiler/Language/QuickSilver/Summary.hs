module Language.QuickSilver.Summary where

import qualified Data.ByteString.Char8 as B

import Data.Binary

import Language.QuickSilver.Syntax
import Language.QuickSilver.Parser.Class
import qualified Language.QuickSilver.Parser.Lex as L
import Language.QuickSilver.Parser
import Language.QuickSilver.PrettyPrint

import Text.Parsec

import System.IO

summaryP :: L.Parser [ClasInterface]
summaryP = many clasInterfaceP

parseSummary :: String -> IO (Either ParseError [ClasInterface])
parseSummary fileName = lexThenParseFromFile summaryP fileName

writeSummary :: FilePath -> [ClasInterface] -> IO ()
writeSummary filePath ifaces = 
  withFile filePath WriteMode $ \ hdl ->
    mapM_ (B.hPutStrLn hdl . B.pack . show . toInterfaceDoc) ifaces

readBinarySummary :: String -> IO [ClasInterface]
readBinarySummary = decodeFile

writeBinarySummary :: String -> [ClasInterface] -> IO ()
writeBinarySummary = encodeFile
