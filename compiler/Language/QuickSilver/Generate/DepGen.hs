module Language.QuickSilver.Generate.DepGen (depGen, depGenInt) where

import Control.Monad
import Control.Monad.Error

import Language.QuickSilver.Syntax
import Language.QuickSilver.Util
import Language.QuickSilver.Parser

import Text.Parsec.Error
import Text.Parsec.Pos

type DepM = ErrorT ParseError IO

instance Error ParseError where
    strMsg s = newErrorMessage (Message s) (initialPos "NoFile") 

depGen :: ClassName -> IO (Either ParseError [Clas])
depGen name = runErrorT $ do
  anyClassEi <- lift (parseClassFile (classNameFile "any"))
  case anyClassEi of
    Left str -> throwError $ strMsg $ "depGen->" ++ show str
    Right anyClass -> depGen' name [anyClass]

depGenInt :: ClassName -> IO (Either ParseError [ClasInterface])
depGenInt = fmap (fmap (map clasInterface)) . depGen

depGen' :: ClassName -> [Clas] -> DepM [Clas]
depGen' cn acc = do
  newClassEi <- lift (parseClassFile (classNameFile cn))
  case newClassEi of
    Left str -> throwError $ strMsg $ "depGen'->" ++ show str
    Right newClass -> depClas newClass (newClass:acc)

depClas :: Clas -> [Clas] -> DepM [Clas]
depClas c acc = 
    depFeats c (acc ++ genericStubs c) >>= depAttrs c >>= depInherit c

depInherit :: Clas -> [Clas] -> DepM [Clas]
depInherit c acc = foldM depTyp acc (map inheritClass $ allInherited c)

depFeats :: Clas -> [Clas] -> DepM [Clas]
depFeats c acc = foldM depFeat acc (allRoutines c)

depAttrs :: Clas -> [Clas] -> DepM [Clas]
depAttrs c acc = depDecls (map attrDecl $ allAttributes c) acc

depFeat :: [Clas] -> Routine -> DepM [Clas]
depFeat acc f = 
    let fSig     = Decl (routineName f) (routineResult f)
        locals   = routineDecls f
        allDecls = fSig : locals ++ routineArgs f
    in depDecls allDecls acc

depDecls :: [Decl] -> [Clas] -> DepM [Clas]
depDecls ds acc = foldM depDecl acc ds

hasClas :: ClassName -> [Clas] -> Bool
hasClas cn = any ( (==) cn . className)

depDecl :: [Clas] -> Decl -> DepM [Clas]
depDecl acc (Decl _ t) = depTyp acc t

depTyp :: [Clas] -> Typ -> DepM [Clas]
depTyp acc (ClassType cn gs)
    | not (hasClas cn acc) = foldM depTyp acc gs >>= depGen' cn
    | otherwise = return acc
depTyp acc (Sep _ _ cn) = depTyp acc (ClassType cn [])
depTyp acc _ = return acc
