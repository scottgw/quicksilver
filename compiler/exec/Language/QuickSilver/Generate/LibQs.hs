-- A module to provide easy access to the libqs functions from
-- our code generation.
{-# LANGUAGE OverloadedStrings #-}
module Language.QuickSilver.Generate.LibQs where

import           Control.Applicative

import           Data.Text (Text)
import qualified Data.Text as Text

import           Language.QuickSilver.Generate.LLVM.Simple
import           Language.QuickSilver.Generate.LLVM.Types

declareQsFuncs :: Build ()
declareQsFuncs =
    do mapM_ genDecl functions
    where
      genDecl (name, resType, argTypes) =
          funcType' resType argTypes >>= addFunc name

      functions = [ ("proc_new", procTypeM, [syncDataTypeM])
                  , ("proc_new_from_other", procTypeM, [procTypeM])
                  , ("proc_new_root"
                    , procTypeM
                    , [ syncDataTypeM
                      , pointer0 <$> funcType' voidTypeM [procTypeM]
                      ]
                    )
                  , ("proc_get_queue", privQueueTypeM, [procTypeM, procTypeM])
                  , ("proc_deref_priv_queues", voidTypeM, [procTypeM])
                  , ("proc_shutdown", voidTypeM, [procTypeM, procTypeM])
                  , ("proc_wait_for_available"
                    , voidTypeM
                    , [procTypeM, procTypeM]
                    )                    
                  , ("closure_new"
                    , closureTypeM
                    , [ voidPtrType
                      , closTypeTypeM
                      , int64TypeM
                      , pointer0 <$> pointer0 <$> pointer0 <$> voidPtrType
                      , pointer0 <$> pointer0 <$> closTypeTypeM
                      ]
                    )
                  , ("closure_pointer_type", closTypeTypeM, [])
                  , ("closure_uint1_type", closTypeTypeM, [])
                  , ("closure_sint8_type", closTypeTypeM, [])
                  , ("closure_sint32_type", closTypeTypeM, [])
                  , ("closure_sint_type", closTypeTypeM, [])
                  , ("closure_void_type", closTypeTypeM, [])

                  , ("priv_queue_new", privQueueTypeM, [procTypeM])
                  , ("priv_queue_lock"
                    , voidTypeM
                    , [privQueueTypeM, procTypeM]
                    )
                  , ("priv_queue_unlock"                    
                    , voidTypeM
                    , [privQueueTypeM, procTypeM]
                    )
                  , ("priv_queue_routine"
                    , voidTypeM
                    , [privQueueTypeM, closureTypeM, procTypeM]
                    )
                  , ("priv_queue_function"
                    , voidTypeM
                    , [privQueueTypeM, closureTypeM, voidPtrType, procTypeM]
                    )
                  , ("priv_queue_access"
                    , voidTypeM
                    , [ privQueueTypeM
                      , closTypeTypeM
                      , voidPtrType
                      , voidPtrType
                      , procTypeM
                      ]
                    )
                  , ("priv_queue_last_was_func", int1TypeM, [privQueueTypeM])
                  , ("priv_queue_shutdown"
                    , voidTypeM
                    , [privQueueTypeM, procTypeM]
                    )

                  , ("create_executors"
                    , voidTypeM
                    , [syncDataTypeM, int64TypeM]
                    )
                  , ("join_executors", voidTypeM, [])

                  , ("notifier_spawn", notifierTypeM, [syncDataTypeM])
                  , ("notifier_join", voidTypeM, [notifierTypeM])

                  , ("sync_data_new", syncDataTypeM, [int64TypeM])
                  , ("sync_data_free", voidTypeM, [syncDataTypeM])
                  ]

voidPtrType :: Build Type
voidPtrType = pointer0 <$> int8TypeM

privQueueTypeM :: Build Type
privQueueTypeM = getOpaqueType "privq"

closureTypeM :: Build Type
closureTypeM = getOpaqueType "closure"

closTypeTypeM :: Build Type
closTypeTypeM = getOpaqueType "clostype"

syncDataTypeM :: Build Type
syncDataTypeM = getOpaqueType "syncdata"

execTypeM :: Build Type
execTypeM = getOpaqueType "exec"

procTypeM :: Build Type
procTypeM = getOpaqueType "proc"

notifierTypeM :: Build Type
notifierTypeM = getOpaqueType "notifier"

getOpaqueType :: Text -> Build Type
getOpaqueType typeName =
    do typePtrMb <- getTypeByName newName
       pointer0 <$>
         case typePtrMb of
           Just typePtr -> return typePtr
           Nothing -> structCreateNamed newName
    where
      newName = typeName `Text.append` "StructType"

funcType' :: Build Type -> [Build Type] -> Build Type
funcType' rM aMs = do
  r <- rM
  as <- sequence aMs
  funcType r as

funcTypeVar' rM aMs = do
  r <- rM
  as <- sequence aMs
  funcTypeVar r as
