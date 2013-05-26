-- A module to provide easy access to the libqs functions from
-- our code generation.
{-# LANGUAGE OverloadedStrings #-}
module Language.QuickSilver.Generate.LibQs where

import           Control.Applicative

import           Data.Text (Text)
import qualified Data.Text as Text

import           Foreign.Ptr

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
                  , ( "proc_wait_for_available"
                    , voidTypeM
                    , [procTypeM, procTypeM]
                    )                    
                  , ( "closure_new"
                    , closureTypeM
                    , [ voidPtrType
                      , closTypeTypeM
                      , int64TypeM
                      , pointer0 <$> pointer0 <$> pointer0 <$> voidPtrType
                      , pointer0 <$> pointer0 <$> closTypeTypeM
                      ]
                    )
                  , ("closure_pointer_type", closTypeTypeM, [])
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

voidPtrType :: Build TypeRef
voidPtrType = pointer0 <$> int8TypeM

privQueueTypeM :: Build TypeRef
privQueueTypeM = getOpaqueType "privq"

closureTypeM :: Build TypeRef
closureTypeM = getOpaqueType "closure"

closTypeTypeM :: Build TypeRef
closTypeTypeM = getOpaqueType "clostype"

syncDataTypeM :: Build TypeRef
syncDataTypeM = getOpaqueType "syncdata"

execTypeM :: Build TypeRef
execTypeM = getOpaqueType "exec"

procTypeM :: Build TypeRef
procTypeM = getOpaqueType "proc"

notifierTypeM :: Build TypeRef
notifierTypeM = getOpaqueType "notifier"

getOpaqueType :: Text -> Build TypeRef
getOpaqueType typeName =
    do typePtr <- getTypeByName newName
       pointer0 <$> if typePtr == nullPtr
                    then structCreateNamed newName
                    else return typePtr
    where
      newName = typeName `Text.append` "StructType"

funcType' :: Build TypeRef -> [Build TypeRef] -> Build TypeRef
funcType' rM aMs = do
  r <- rM
  as <- sequence aMs
  funcType r as

funcTypeVar' rM aMs = do
  r <- rM
  as <- sequence aMs
  funcTypeVar r as
