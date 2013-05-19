{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Language.QuickSilver.Generate.Memory.Type
    ( genClassStructs
    , modClas
    , modFeatureArgs
    , featureAsCreate
    , mkCreateFunc
    ) where

import Control.Applicative
import Control.Lens
import Control.Monad
import Control.Monad.Reader

import qualified Data.HashMap.Strict as Map
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Traversable as Traverse

import Language.QuickSilver.Syntax
import Language.QuickSilver.Util

import Language.QuickSilver.Generate.Memory.Class
import Language.QuickSilver.Generate.Memory.Feature

import Language.QuickSilver.Generate.LLVM.Simple
import Language.QuickSilver.Generate.LLVM.Types
import Language.QuickSilver.Generate.LLVM.Util

-- Generate the class structres and the routine prototypes
-- so they are available to be looked up later.
genClassStructs :: [ClasInterface] -> Build ClassEnv
genClassStructs = opaqueClasses >=> constructClassTypes

nameClassInfo :: ClasInterface -> Build (Text, ClassInfo)
nameClassInfo clsIntr = (name,) <$> ClassInfo clsIntr <$> t    
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
setClasType pcMap (ClassInfo cls t) =
  local (setClassEnv pcMap) $ do
    -- Non special classes need their struct generated.
    -- special classes have already been done in 'nameClassInfo'
    t' <- if (not $ isSpecialClass cls) 
          then
            do ts <- unClasTable <$> mkClasTable cls
               structSetBody t ts False
               return (pointer0 t)
          else return t
    return (ClassInfo cls t')

isSpecialClass :: ClasInterface -> Bool
isSpecialClass cls = view className cls `elem` map fst nameAndType

mkSpecialClassType :: Text -> Build TypeRef
mkSpecialClassType name = 
    case lookup name nameAndType of
      Just t -> t
      Nothing ->
          error $ "mkSpecialClassType: non special type: " ++ Text.unpack name

nameAndType :: [(Text, Build TypeRef)]
nameAndType =
    [("Pointer_8", pointer0 <$> int8TypeM)
    ]


-- Adding first `Current' argument to functions
modClas :: ClasInterface -> ClasInterface
modClas c
  | view isModule c = c
  | otherwise = classMapRoutines (modFeatureArgs c) c

modFeatureArgs :: AbsClas body expA
               -> AbsRoutine body expB -> AbsRoutine body expB
modFeatureArgs ci feat =
    let cName     = view className ci
        ts        = map (flip ClassType [] . genericName) (view generics ci)
        currDecl  = Decl "Current" (ClassType cName ts)
        args      = currDecl : (routineArgs feat)
    in  feat {routineArgs = args}

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
