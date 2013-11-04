module Language.QuickSilver.TypeCheck.TypeError where

import           Control.Monad.Error
import           Language.QuickSilver.Syntax
import           Language.QuickSilver.Position

data TypeError
  = MiscError String
  | IdentNotFound String
  | MissingCall Typ String
  | MissingInfix String Typ Typ
  | BasicQualCall
  | ArgNumDiffer Int Int
  | TypeMismatch { teExpected :: Typ
                 , teActual :: Typ
                 }
  | CurrentInModule
  | SeparateBlockArg
  | SeparateShutdown
  | PassiveBlockReserve
  | CreateNoType
  | ErrorWithPos SourcePos TypeError
  deriving Show

instance Error TypeError where
  strMsg = MiscError
