-- A module to provide easy access to the libqs functions from
-- our code generation.
{-# LANGUAGE OverloadedStrings #-}
module Language.QuickSilver.Generate.LibQs where

import           Control.Applicative

import           Data.Text (Text)
import qualified Data.Text as Text

import           Language.QuickSilver.Generate.LLVM.Simple
import           Language.QuickSilver.Generate.LLVM.Build

declareQsFuncs :: Build ()
declareQsFuncs =
    do mapM_ genDecl functions
    where
      genDecl (name, resType, argTypes) =
          funcType' resType argTypes >>= addFunction name

      functions = [ ("proc_new", procTypeM, [syncDataTypeM])
                  , ("proc_new_from_other", procTypeM, [procTypeM])
                  , ("proc_new_root"
                    , procTypeM
                    , [ syncDataTypeM
                      , pointer0 <$> funcType' voidType [procTypeM]
                      ]
                    )
                  , ("proc_get_queue", privQueueTypeM, [procTypeM, procTypeM])
                  , ("proc_shutdown", voidType, [procTypeM, procTypeM])
                  , ("proc_wait_for_available"
                    , voidType
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
                  , ("closure_double_type", closTypeTypeM, [])
                  , ("closure_void_type", closTypeTypeM, [])

                  , ("priv_queue_new", privQueueTypeM, [procTypeM])
                  , ("priv_queue_lock"
                    , voidType
                    , [privQueueTypeM, procTypeM]
                    )
                  , ("priv_queue_unlock"                    
                    , voidType
                    , [privQueueTypeM, procTypeM]
                    )
                  , ("priv_queue_set_in_wait"
                    , voidType
                    , [privQueueTypeM]
                    )
                  , ("priv_queue_set_in_body"
                    , voidType
                    , [privQueueTypeM]
                    )
                  , ("priv_queue_routine"
                    , voidType
                    , [privQueueTypeM, closureTypeM, procTypeM]
                    )
                  , ("priv_queue_function"
                    , voidType
                    , [privQueueTypeM, closureTypeM, voidPtrType, procTypeM]
                    )
                  , ("priv_queue_access"
                    , voidType
                    , [ privQueueTypeM
                      , closTypeTypeM
                      , voidPtrType
                      , voidPtrType
                      , procTypeM
                      ]
                    )
                  , ("priv_queue_last_was_func", int1TypeM, [privQueueTypeM])
                  , ("priv_queue_sync", voidType, [privQueueTypeM, procTypeM])

                  , ("sync_data_create_executors"
                    , voidType
                    , [syncDataTypeM, int64TypeM]
                    )
                  , ("sync_data_join_executors", voidType, [syncDataTypeM])

                  , ("notifier_spawn", notifierTypeM, [syncDataTypeM])
                  , ("notifier_join", voidType, [notifierTypeM])

                  , ("sync_data_new", syncDataTypeM, [int64TypeM])
                  , ("sync_data_free", voidType, [syncDataTypeM])

                  , ("sync_data_io_mgr_join", voidType, [syncDataTypeM])
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
  return (funcType r as)

funcTypeVar' rM aMs = do
  r <- rM
  as <- sequence aMs
  return (funcTypeVar r as)
