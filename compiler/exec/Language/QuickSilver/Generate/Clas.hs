{-# LANGUAGE OverloadedStrings #-}
module Language.QuickSilver.Generate.Clas (genClass) where

import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.Reader

import Data.Text (Text)
import qualified Data.Text as Text

import Language.QuickSilver.Syntax
import Language.QuickSilver.Util
import Language.QuickSilver.TypeCheck.TypedExpr

-- import Language.QuickSilver.Generate.Builtin.Builtins
import Language.QuickSilver.Generate.Eval
import Language.QuickSilver.Generate.Feature
import Language.QuickSilver.Generate.Memory.Class
import Language.QuickSilver.Generate.Memory.Type
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
  genRoutines (view routines clas)
  debug "Generating main routine (if required)"
  when isMain (genMain clas)

genMain :: TClass -> Build ()
genMain clas = do
  let (<#>) = callByName
  i64T <- int64TypeM
  ptrT <- ptr
  fRef <- addFunc "main" =<< funcType i64T []
  atNewBlock fRef "mainStart"

  call "GC_init" []
  let mainName  = fullNameStr (view className clas) "main"

  mainFunc <- getNamedFunction mainName

  maxProcs <- int 32000
  numExecs <- int 4

  syncData <- "sync_data_new" <#> [maxProcs]
  rootProc <- "make_root_processor" <#> [syncData, mainFunc]
  "create_executors" <#> [syncData, numExecs]

  notifier <- "notifier_spawn" <#> [syncData]
  "notifier_join" <#> [notifier]

  "join_executors" <#> []
  "sync_data_free" <#> [syncData]

  zero <- int 0
  ret zero

-- -- Old exception handling code, put aside for the moment to do
-- -- QuickSilver calls.
--   norm <- appendBasicBlock fRef "normal"
--   lpad <- appendBasicBlock fRef "landing pad"
--   let mainName -- | view isModule clas = "main"
--                -- | otherwise
--                    = fullNameStr (view className clas) "main"
--   mainArg <- if view isModule clas
--              then return []
--              else 
--                  do curr <- unClasRef <$> mallocClas (view className clas)
--                     return [curr]
--   f <- getNamedFunction mainName
--   r <- invoke' f mainArg norm lpad "make"
--   -- setInstructionCallConv r Fast

--   positionAtEnd lpad
--   -- ex <- call "llvm.eh.exception" []
--   pf <- getNamedFunction "__gxx_personality_v0"
--   -- pfCast <- bitcast pf ptrT ""
--   tuple <- structType [ptrT, i64T] False
--   landRes <- buildLandingPad tuple pf 1 "my landing pad"
--   zti <- lookupEnv "_ZTIi"
--   ztiCast <- bitcast zti ptrT ""
--   addClause landRes ztiCast
--   ex <- extractValue landRes 0

--   -- call "llvm.eh.selector.i32" [ex, pfCast, ztiCast, nul ptr]
--   call "llvm.eh.typeid.for" [ztiCast]
  
--   call "__cxa_begin_catch" [ex]
--   call "__cxa_end_catch" []
--   zero <- int 0
--   ret zero
  
--   positionAtEnd norm
--   ret zero

genCreates :: [Text] -> Build ()
genCreates = mapM_ genCreate

genCreate :: Text -> Build ()
genCreate fName = do
  cName <- view className `fmap` currentClass
  let featCreat = featureAsCreate cName fName
  createFunc <- getNamedFunction featCreat
  atNewBlock createFunc (featCreat `Text.append` " start")
  -- v <- if needsBuiltinCreate cName fName
  --   then genBuiltinCreate cName fName
  --   else unClasRef <$> mallocClas cName
  v <- unClasRef <$> mallocClas cName
  ret v

atNewBlock :: ValueRef -> Text -> Build ()
atNewBlock f str = appendBasicBlock f str >>= positionAtEnd
