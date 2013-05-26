{-# LANGUAGE OverloadedStrings #-}
module Language.QuickSilver.Generate.Clas (genClass) where

import Control.Lens
import Control.Monad
import Control.Monad.Reader

import Data.Text (Text)

import Language.QuickSilver.Syntax
import Language.QuickSilver.Util
import Language.QuickSilver.TypeCheck.TypedExpr
import Language.QuickSilver.Generate.Eval
import Language.QuickSilver.Generate.Routine
import Language.QuickSilver.Generate.Memory.Declarations
import Language.QuickSilver.Generate.Preamble
import Language.QuickSilver.Generate.LLVM.Simple
import Language.QuickSilver.Generate.LLVM.Util
import Language.QuickSilver.Generate.LLVM.Types

genClass :: TClass -> Bool -> Build ()
genClass clas isMain = do
  envTransform <- preamble clas 
  let clas' = classMapRoutines (modTRoutineArgs clas) clas
  local envTransform (genClass' clas' isMain)

genClass' :: TClass -> Bool -> Build ()
genClass' clas isMain = do
  -- Note: generating creation routines specially is required when we have
  -- special classes that have to be created in an interesting way.
  -- This is not the case for regular classes, so we'll try to get away
  -- without doing this!
  
  -- debug "Generating Creation routines"
  -- genCreates (allCreates clas)
  debug "Generating routines"
  genRoutines isMain (view routines clas)
  debug "Generating main routine (if required)"
  when isMain (genMain clas)

genMain :: TClass -> Build ()
genMain clas = do
  i64T <- int64TypeM
  fRef <- addFunc "main" =<< funcType i64T []
  atNewBlock fRef "mainStart"

  call "qs_init" []
  let mainName  = fullNameStr (view className clas) "main"

  mainFunc <- getNamedFunction mainName

  maxProcs <- int 32000
  numExecs <- int 4

  syncData <- "sync_data_new" <#> [maxProcs]
  rootProc <- "proc_new_root" <#> [syncData, mainFunc]
  "create_executors" <#> [syncData, numExecs]

  notifier <- "notifier_spawn" <#> [syncData]
  "notifier_join" <#> [notifier]

  "join_executors" <#> []
  "sync_data_free" <#> [syncData]

  zero <- int 0
  ret zero

atNewBlock :: ValueRef -> Text -> Build ()
atNewBlock f str = appendBasicBlock f str >>= positionAtEnd
