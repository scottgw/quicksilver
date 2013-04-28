{-# LANGUAGE ScopedTypeVariables #-}

module Language.QuickSilver.Generate.Memory.Type
    ( genClassStructs
    , modClas
    , modFeatureArgs
    , featureAsCreate
    ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Reader

import qualified Data.Map as Map
import qualified Data.Traversable as Traverse

import Language.QuickSilver.Syntax
import Language.QuickSilver.Util

import Language.QuickSilver.Generate.Memory.Class
import Language.QuickSilver.Generate.Memory.Feature

import Language.QuickSilver.Generate.LLVM.Simple
import Language.QuickSilver.Generate.LLVM.Types
import Language.QuickSilver.Generate.LLVM.Util

genClassStructs :: [ClasInterface] -> Build ClassEnv
genClassStructs = opaqueClasses >=> constructClassTypes

nameClassInfo :: ClasInterface -> Build (String, ClassInfo)
nameClassInfo clsIntr = 
    let name = className clsIntr 
        err = error $ "dummyVtable: " ++ name
    in do ci <- ClassInfo clsIntr <$> structCreateNamed name <*> pure err
          return (name, ci)

opaqueClasses :: [ClasInterface] -> Build ClassEnv
opaqueClasses csNormal =
  let cs = map modClas (csNormal ++ map makeGenericStub (generics $ head csNormal))
  in Map.fromList <$> mapM nameClassInfo cs

mkVTable :: ClasInterface -> Build ValueRef
mkVTable c = 
    let featureFunc f  = do
          fType <- featDeclType f
          let name = fullNameStr (className c) (featureName f)
          fPtr <- addFunc name fType
          debug $ "mkVTable adding function " ++ name ++ " @ " ++ show fPtr
          return fPtr
        featureFuncs   = mapM featureFunc (allFeatures c)
        createFuncs    = filter (flip isCreateName c . featureName) 
                                (allFeatures c)
    in do
      mapM_ (mkCreateFunc c) createFuncs
      vtVal <- struct False =<< featureFuncs
      vtTyp <- typeOfVal vtVal
      vtRef <- addGlobal vtTyp ("vtable" ++ className c)

      setLinkage vtRef 4
      setInitializer vtRef vtVal

      return vtRef

mkCreateFunc :: ClasInterface -> RoutineI -> Build ()
mkCreateFunc c f = featDeclType f' >>= addFunc crName >> return ()
    where crName = featureAsCreate (className c) (featureName f)
          f' = f {routineName = crName
                 ,routineResult = ClassType (className c) [ClassType "G" []]
                 ,routineArgs = tail (routineArgs f)
                 }

featureAsCreate :: String -> String -> String
featureAsCreate cName fName = "__" ++ cName ++ fName ++ "_create"

insertVTable :: ClassEnv -> Build ClassEnv
insertVTable pcMap = 
    let updPartVTable (ClassInfo c t _) = ClassInfo c <$> pure t <*> mkVTable c
    in Traverse.mapM (local (setClassEnv pcMap) . updPartVTable) pcMap
     
constructClassTypes :: ClassEnv -> Build ClassEnv
constructClassTypes pcMap = do
  Traverse.mapM (setClasType pcMap) pcMap
  insertVTable pcMap

-- bug here, evaluating the vtable somewhere
setClasType :: ClassEnv -> ClassInfo -> Build ()
setClasType pcMap (ClassInfo cls t _) =
  local (setClassEnv pcMap) $ do
    ts <- unClasTable <$> mkClasTable cls
    structSetBody t ts False

-- Adding first `Current' argument to functions
modClas :: ClasInterface -> ClasInterface
modClas c = classMapRoutines (modFeatureArgs c) c

modFeatureArgs :: AbsClas body expA
               -> AbsRoutine body expB -> AbsRoutine body expB
modFeatureArgs ci feat =
    let cName     = className ci
        ts        = map (flip ClassType [] . genericName) (generics ci)
        currDecl  = Decl "Current" (ClassType cName ts)
        args      = currDecl : (routineArgs feat)
    in  feat {routineArgs = args}
