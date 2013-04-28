module Language.QuickSilver.Generate.Interface where

import Language.QuickSilver.Syntax

import Parser.Parser (classNameFile)

extractInterface :: AbsClas body exp -> ClasInterface
extractInterface c = c {features = map makeFeatureI (features c), invnts = []}

showInterface :: ClasInterface -> String
showInterface c = 
    unlines $ concat
      [["class " ++ className c], [[]]
      ,["feature"]
      ,map show (attributes c), [[]]
      ,map show (features c)
      ,["end"]
      ]

reduceToInterfaceStr :: ClasI exp -> String
reduceToInterfaceStr = showInterface . extractInterface

interfaceFilename :: ClasInterface -> String
interfaceFilename ci = classNameFile (className ci) ++ "i"

writeInterface :: ClasInterface -> IO ()
writeInterface ci =
  writeFile (interfaceFilename ci) (showInterface ci)
