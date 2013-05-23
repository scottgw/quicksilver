{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Language.QuickSilver.Generate.Memory.Declarations
    ( genClassStructs
    , modClas
    , modTRoutineArgs
    , featureAsCreate
    , mkCreateFunc
    ) where

import Control.Applicative
import Control.Lens hiding (pre)
import Control.Monad
import Control.Monad.Reader

import qualified Data.HashMap.Strict as Map
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Traversable as Traverse

import Language.QuickSilver.Syntax
import Language.QuickSilver.Util
import Language.QuickSilver.TypeCheck.TypedExpr
import Language.QuickSilver.Generate.LibQs
import Language.QuickSilver.Generate.Memory.Types
import Language.QuickSilver.Generate.LLVM.Simple
import Language.QuickSilver.Generate.LLVM.Types
import Language.QuickSilver.Generate.LLVM.Util

-- Generate the class structres and the routine prototypes
-- so they are available to be looked up later.
genClassStructs :: [ClasInterface] -> Build ClassEnv
genClassStructs = opaqueClasses >=> constructClassTypes

nameClassInfo :: ClasInterface -> Build (Text, ClassInfo)
nameClassInfo clsIntr = (name,) <$> ClassInfo clsIntr <$> Left <$> t
    where name = view className clsIntr 
          t | isSpecialClass clsIntr = mkSpecialClassType name
            | otherwise              = structCreateNamed name

opaqueClasses :: [ClasInterface] -> Build ClassEnv
opaqueClasses csNormal =
  let cs = map modClas (csNormal ++ map makeGenericStub (view generics $ head csNormal))
  in Map.fromList <$> mapM nameClassInfo cs

constructClassTypes :: ClassEnv -> Build ClassEnv
constructClassTypes pcMap =
  do pcMap' <- Traverse.mapM (setClasType pcMap) pcMap
     Traverse.mapM (setupRoutines pcMap') pcMap'
     return pcMap'

setupRoutines :: ClassEnv -> ClassInfo -> Build ()
setupRoutines pcMap (ClassInfo cls _t) = 
    local (setClassEnv pcMap) $ mapM_ genRoutineType (view routines cls)    
    where
      genRoutineType rtn =
          do fType <- featDeclType rtn
             let name = fullNameStr (view className cls) (routineName rtn)

             -- External routines should also declare the external
             -- thing they bind to as a function. The type of that
             -- function matches the type of the outer declaration,
             -- so we just reuse it.
             case routineImpl rtn of
                       EmptyExternal extern _ -> void (addFunc extern fType)
                       _ -> return ()
             fPtr <- addFunc name fType
             debug $ concat [ "Adding routine prototype "
                            , Text.unpack name ," @ ", show fPtr
                            ]
             return fPtr

setClasType :: ClassEnv -> ClassInfo -> Build ClassInfo
setClasType pcMap (ClassInfo cls (Left t)) =
  local (setClassEnv pcMap) $ do
    -- Non special classes need their struct generated.
    -- special classes have already been done in 'nameClassInfo'
    t' <- if (not $ isSpecialClass cls) 
          then
            do ts <- unClasTable <$> mkClasTable cls
               structSetBody t ts False
               return (pointer0 t)
          else return t
    return (ClassInfo cls (Right t'))
setClasType _pcMap (ClassInfo _cls (Right _t)) =
    error "setClasType: found 'Right', should not be possible"

-- Adding first `Current' argument to functions
modClas :: ClasInterface -> ClasInterface
modClas c = classMapRoutines (modRoutineIArgs c) c

modTRoutineArgs :: TClass -> TRoutine -> TRoutine
modTRoutineArgs  = modRoutineArgs go
    where
      go rout p = case routineImpl rout of
               RoutineExternal _ _ -> []
               _ -> [p]

modRoutineIArgs :: AbsClas EmptyBody expA
               -> AbsRoutine EmptyBody expB -> AbsRoutine EmptyBody expB
modRoutineIArgs = modRoutineArgs go
    where
      go rout p = case routineImpl rout of
               EmptyExternal _ _ -> []
               _ -> [p]

modRoutineArgs go ci rout =
    let cName     = view className ci
        isMod     = view isModule ci
        ts        = map (flip ClassType [] . genericName) (view generics ci)
        currDecl  = Decl "Current" (ClassType cName ts)
        procDecl  = Decl "<CurrentProc>" ProcessorType
        pre | isMod     = go rout procDecl
            | otherwise = go rout procDecl ++ [currDecl]
    in rout {routineArgs = pre ++ routineArgs rout}

-- Creation routines
mkCreateFunc :: ClasInterface -> RoutineI -> Build ()
mkCreateFunc c f = featDeclType f' >>= addFunc crName >> return ()
    where crName = featureAsCreate (view className c) (routineName f)
          f' = f {routineName = crName
                 ,routineResult = ClassType (view className c) [ClassType "G" []]
                 ,routineArgs = tail (routineArgs f)
                 }

featureAsCreate :: Text -> Text -> Text
featureAsCreate cName fName = Text.concat ["__", cName, fName, "_create"]


newtype ClasTable = ClasTable {unClasTable :: [TypeRef]}

mkClasTable :: ClasInterface -> Build ClasTable
mkClasTable c = 
    ClasTable <$> mapM (typeOfDecl . attrDecl) (view attributes c)
