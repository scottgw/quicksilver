module Language.QuickSilver.Generate.Interface where

import Control.Lens

import Data.Text (unpack)

import Language.QuickSilver.Syntax
import Language.QuickSilver.Parser (classNameFile)
import Language.QuickSilver.Util


extractInterface :: AbsClas body Expr -> ClasInterface
extractInterface c =
  c { _routines = map makeRoutineI (_routines c)
    , _invnts   = []
    }

showInterface :: ClasInterface -> String
showInterface c = 
    unlines $ concat
      [["class " ++ unpack (view className c)], [[]]
      ,["feature"]
      ,map show (view attributes c), [[]]
      ,map show (view routines c)
      ,["end"]
      ]

reduceToInterfaceStr :: ClasI Expr -> String
reduceToInterfaceStr = showInterface . extractInterface

interfaceFilename :: ClasInterface -> String
interfaceFilename ci = classNameFile (view className ci) ++ "i"

writeInterface :: ClasInterface -> IO ()
writeInterface ci =
  writeFile (interfaceFilename ci) (showInterface ci)
