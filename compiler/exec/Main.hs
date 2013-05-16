{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Control.Lens
import Control.Monad

import Data.Char (toLower)
import qualified Data.Text as Text

import System.Console.CmdArgs

import Language.QuickSilver.Syntax
import Language.QuickSilver.Parser (parseClassFile)

import Language.QuickSilver.TypeCheck.Class

import Language.QuickSilver.Generate.DepGen
import Language.QuickSilver.Generate.CompilePipeline (compileIO)


main :: IO ()
main = do
  argu <- cmdArgs compMode
  let file = head . compFile $ argu
      outFileM = output argu

  parsedClass <- parseClassFile file
  either print (compile outFileM (isDebug argu) (isMain argu) (isFast argu)) parsedClass

compMode :: Args
compMode = CompArgs
           {
             compFile = def &= args
           , output   = def &= explicit &= name "o"
           , isFast   = def &= explicit &= name "f"
           , isMain   = def &= explicit &= name "m" &= name "main"
           , isLink   = def &= explicit &= name "l" &= name "link"
           , isDebug  = def &= explicit &= name "d" &= name "debug"
           }
             
data Args 
    = CompArgs 
      { compFile :: [String]
      , output   :: String
      , isFast   :: Bool
      , isMain   :: Bool
      , isLink   :: Bool
      , isDebug  :: Bool
      } 
    deriving (Show, Data, Typeable)

clasFile :: Clas -> String
clasFile = (++ ".bc") . map toLower . Text.unpack . view className

compile :: String -> Bool -> Bool -> Bool -> Clas -> IO ()
compile outFile debug genMain runMain cls = do
    let outFile' = case outFile of
                    "" -> clasFile cls
                    str -> str

    when debug $ putStrLn "Generating dependency interfaces"
    envsE <- depGenInt (view className cls)
    when debug $ print envsE
    case envsE of
      Left e -> putStrLn "Dependency Error:" >> error (show e)
      Right envs ->
          case clasM envs cls of
            Left  e -> putStrLn "TypeChecking Error:" >> error e
            Right c -> 
                if not runMain
                then compileIO debug outFile' genMain c
                else return ()
