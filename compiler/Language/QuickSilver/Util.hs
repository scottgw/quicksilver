{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.QuickSilver.Util where

import           Control.Applicative hiding (getConst)
import           Control.Lens hiding (from, lens)

import           Data.Generics
import qualified Data.HashMap.Strict as Map
import           Data.List
import           Data.Maybe
import qualified Data.Text as Text
import           Data.Text (Text)

import           Language.QuickSilver.Syntax

-- Class level utilities

-- | Determine if a name is in the creation clause of a class.
isCreateName n c = n `elem` allCreates c

-- | Fetch creation routines from all feature clauses.
allCreates = concatMap createNames . view creates


-- * Class modification

-- ** Setting members of a class.

-- | Map a transformation function over the attributes in a class, replacing the
-- old attributes with the transformed versions within a class.
classMapAttributes f = over (attributes.traverse) f

-- | Monadic version of 'classMapAttributes'.
classMapAttributesM :: (Applicative m, Monad m) =>
                       (Attribute -> m Attribute) ->
                       AbsClas body exp -> 
                       m (AbsClas body exp)
classMapAttributesM f = mapMOf (attributes.traverse) f

-- | Map a transformation function over the routines in a class, replacing the
-- old routines with the transformed versions within a class.
classMapRoutines :: (AbsRoutine body exp -> AbsRoutine body exp) 
                    -> AbsClas body exp -> AbsClas body exp
classMapRoutines f = over (routines.traverse) f

-- | Monadic version of 'classMapRoutines'.
classMapRoutinesM :: (Applicative m, Monad m) =>
                     (AbsRoutine body exp -> m (AbsRoutine body exp)) ->
                     AbsClas body exp -> 
                     m (AbsClas body exp)
classMapRoutinesM f = mapMOf (routines.traverse) f

-- | Map a transformation function over the constants in a class, replacing the
-- old constants with the transformed versions within a class.
classMapConstants f = over (consts.traverse) f

-- | Map a transformation function over all expressions in a class. 
-- A transformation for features, constants, and attributes must be given
-- as if the type of expressions changes (ie, with a typecheck) then
-- all expressions types must change together. This is performed on every
-- feature clause in a class.
classMapExprs :: (AbsRoutine body exp -> AbsRoutine body' exp') 
                 -> (Clause exp -> Clause exp')
                 -> (Constant exp -> Constant exp')
                 -> AbsClas body exp -> AbsClas body' exp'
classMapExprs routineF clauseF constF c = 
  c { _routines = map routineF (view routines c)
    , _consts   = map constF   (view consts c)
    , _invnts   = map clauseF  (view invnts c)
    }

-- * Interface construction

-- | Strip the bodies from all features.
clasInterface :: AbsClas (RoutineBody expr) Expr -> ClasInterface
clasInterface = over (routines.traverse) makeRoutineI

-- | Strip the bodies and rescue clause from a routine.
makeRoutineI :: AbsRoutine (RoutineBody expr) Expr -> RoutineI
makeRoutineI rtn = 
    rtn { routineImpl = emp
        , routineRescue = Nothing
        }
    where
      emp = case routineImpl rtn of
              RoutineExternal name body -> EmptyExternal name body
              _ -> EmptyBody

-- * Map construction

-- | Turn a list of classes into a map indexed by the class names.
clasMap :: [AbsClas body exp] -> Map ClassName (AbsClas body exp)
clasMap = Map.fromList . map (\ c -> (view className c, c))

-- | Extract a map of attribute names to types given a class.
attrMap :: AbsClas body exp -> Map Text Typ
attrMap = declsToMap . map attrDecl . view attributes

-- * Search

-- | Find a routine in a class.
findRoutine :: Clas -> Text -> Maybe Routine
findRoutine = findAbsRoutine

findAbsRoutine :: AbsClas body expr -> Text -> Maybe (AbsRoutine body expr)
findAbsRoutine ci name = find ((== name) . routineName) (view routines ci)


-- | Find an operator (symbol sequence) in a class.
findOperator :: AbsClas body Expr -> Text -> Int -> 
                Maybe (AbsRoutine body Expr)
findOperator c opName numArgs =
    let fs = view routines c
        ffs = filter (\ rout -> routineAlias rout == Just opName &&
                                length (routineArgs rout) == numArgs) fs
    in listToMaybe ffs

-- | Find a routine by name.
findRoutineInt :: ClasInterface -> Text -> Maybe RoutineI
findRoutineInt = findAbsRoutine

-- | Find an attribute in a class by name.
findAttrInt :: AbsClas body expr -> Text -> Maybe Attribute
findAttrInt ci name = 
  find ((== name) . declName . attrDecl) (view attributes ci)

-- | Find a constant by name in a class.
findConstantInt :: AbsClas body Expr -> Text -> Maybe (Constant Expr)
findConstantInt ci name = 
  find ((== name) . declName . constDecl) (view consts ci)

-- | Given a class and a routine, given a unique name.
fullName :: AbsClas body exp -> RoutineI -> Text
fullName c f = fullNameStr (view className c) (routineName f)

-- | Given to string construct a unique combination.
fullNameStr :: Text -> Text -> Text
fullNameStr cName fName = Text.concat ["__", cName, "_", fName]

-- | Given a class, create a list of generic classes for the formal generic  
-- parameters of the class.
genericStubs :: AbsClas body exp -> [AbsClas body' exp']
genericStubs = map makeGenericStub . view generics

-- | Given an AnyRefType, construct a class for the name.
makeGenericStub :: Typ -> AbsClas body exp
makeGenericStub (AnyRefType name) = 
  AbsClas { _className  = name
          , _imports    = []
          , _isModule   = False
          , _generics   = []
          , _creates    = []
          , _attributes = [] 
          , _routines   = []
          , _consts     = []
          , _invnts     = []
          }
makeGenericStub t = error $ "makeGenericStub: cannot make stub from " ++ show t
-- * Routine level utilities

-- | Construct a map from a routine's arguments.
argMap :: RoutineWithBody a -> Map Text Typ
argMap = declsToMap . routineArgs

-- | Construct a map from a routine's declarations.
localMap :: RoutineWithBody a -> Map Text Typ
localMap = declsToMap . routineDecls

-- | Give the declarations of a routine's locals.
routineDecls :: AbsRoutine (RoutineBody exp1) exp -> [Decl]
routineDecls r =
  case routineImpl r of
    RoutineExternal _ _ -> []
    body -> routineLocal body

routineIsExternal :: AbsRoutine (RoutineBody exp) exp -> Bool
routineIsExternal rtn =
    case routineImpl rtn of
      RoutineExternal _ _ -> True
      _ -> False

-- Operator utilities

-- | Operator aliases for user-level operators, ie, not including
-- =, /=, ~, and /~
opAlias :: BinOp -> Text
opAlias Add = "+"
opAlias Sub = "-"
opAlias Mul = "*"
opAlias Div = "/"
opAlias Quot = "//"
opAlias Rem = "\\"
opAlias Pow = "^"
opAlias And = "and"
opAlias AndThen = "and then"
opAlias Or = "or"
opAlias OrElse = "or else"
opAlias Xor = "xor"
opAlias Implies = "implies"
opAlias (SymbolOp o) = o
opAlias (RelOp o _) = rel o
  where
    rel Lte = "<="
    rel Lt = "<"
    rel Gt = ">"
    rel Gte = ">="    
    rel relOp = error $ "opAlias: non user-level operator " ++ show relOp

-- | Test if the binary operator is an equality operator.
equalityOp :: BinOp -> Bool
equalityOp (RelOp Eq _) = True
equalityOp (RelOp Neq _) = True
equalityOp _ = False


-- | Unary operator aliases for everything except `old'.
unOpAlias Not = "not"
unOpAlias Neg = "-"
unOpAlias Old = "unOpAlias: `old' is not a user-operator."


-- * Type utilities

-- | Convert a class into its type.
classToType :: AbsClas body exp -> Typ
classToType clas = ClassType (view className clas) (view generics clas)

-- | Whether a type is basic (where basic meaning its an integer, natural, real or boolean).
isBasic :: Typ -> Bool
isBasic t = case t of
              BoolType -> True
              AnyIntType -> True
              Int8Type -> True
              Int16Type -> True
              Int32Type -> True
              Int64Type -> True
              Natural8Type -> True
              Natural16Type -> True
              Natural32Type -> True
              Natural64Type -> True
              CharType -> True
              DoubleType -> True
              _ -> False

-- | Is this a separate type?
isSeparate :: Typ -> Bool
isSeparate (Sep _ _ _) = True
isSeparate _ = False

-- | Is this any reference type?
isAnyRefType :: Typ -> Bool
isAnyRefType (AnyRefType _) = True
isAnyRefType _ = False

-- | A list of the number of integer bits (8, 16, ...)
intBits :: [Integer]
intBits = [8, 16, 32, 64]


-- | The bounds on the range of values a integer or natural type can take.
typeBounds :: Typ -> (Integer, Integer)
typeBounds (ClassType n []) = fromJust $ lookup n wholeMap
  where
    intMap = zip integerTypeNames 
                 (map (\bits -> let half = bits `quot` 2
                                in (- 2^half, 2^half - 1)) intBits)
    natMap = zip naturalTypeNames 
                 (map (\bits -> (0, 2^bits - 1)) intBits)
    wholeMap = intMap ++ natMap
typeBounds t = error $ "typeBounds: won't work on " ++ show t

-- | Boolean type test.
isBooleanType :: Typ -> Bool
isBooleanType = (== "BOOLEAN") . classTypeName

-- | Integer type test.
isIntegerType :: Typ -> Bool
isIntegerType t = 
    case t of
      AnyIntType -> True
      Int8Type -> True
      Int32Type -> True
      Int64Type -> True
      Natural8Type -> True
      Natural32Type -> True
      Natural64Type -> True
      _ -> False

-- | Natural number type test.
isNaturalType :: Typ -> Bool
isNaturalType = isInTypeNames naturalTypeNames

-- | Real number type test.
isRealType :: Typ -> Bool
isRealType = isInTypeNames realTypeNames

-- | Character type test.
isCharType :: Typ -> Bool
isCharType = isInTypeNames charTypeNames

isInTypeNames names (ClassType name _) = name `elem` names
isInTypeNames _ _ = False

-- | List of integer type names (ie, INTEGER_32).
integerTypeNames :: [Text]
integerTypeNames = map ((Text.append "INTEGER_") . Text.pack . show) intBits

-- | List of integer type names (ie, NATURAL_32).
naturalTypeNames :: [Text]
naturalTypeNames = map ((Text.append "NATURAL_") . Text.pack . show) intBits

-- | List of integer type names (ie, REAL_64).
realTypeNames :: [Text]
realTypeNames = ["REAL_32", "REAL_64"]

-- | List of integer type names (ie, CHARACTER_8).
charTypeNames :: [Text]
charTypeNames = ["CHARACTER_8", "CHARACTER_32"]

-- | Given a type give the name of the class as a string.
classTypeName :: Typ -> Text
classTypeName (ClassType cn _) = cn 
classTypeName (Sep _ _ t) = classTypeName t
classTypeName t = error $ "Non-class type " ++ show t

-- | The default integer type.
intType :: Typ
intType = Int64Type

-- | The default natural type.
natType :: Typ
natType = Natural64Type

-- | The default boolean type.
boolType :: Typ
boolType = BoolType

-- | The default real number type.
realType :: Typ
realType = DoubleType

-- | The default character type.
charType :: Typ
charType = CharType

-- | The default string type.
stringType :: Typ
stringType = namedType "String"

-- | The top type, ANY.
anyType :: Typ
anyType = namedType "ANY"
  
-- | Construct a simple type from a classname.
namedType :: ClassName -> Typ
namedType name = ClassType name []



type GenUpd a = Typ -> Typ -> a -> a

-- | Replace one type with another in a class.type in a class.
replaceType :: (Data body, Typeable body) => GenUpd (AbsClas body Expr)
replaceType oldType newType = everywhere (mkT updateType)
    where
      updateType t
          | t == oldType = newType
          | otherwise =
              case t of
                ClassType name types -> ClassType name (map updateType types)
                _ -> t


-- * Declaration

-- | Insert a declaration into a string-type map.
insertDecl :: Decl -> Map Text Typ -> Map Text Typ
insertDecl (Decl s t) = Map.insert s t

-- | Turn a list of declarations into a string-type map.
declsToMap :: [Decl] -> Map Text Typ
declsToMap = foldr insertDecl Map.empty

-- * SCOOP utilities

-- | Given a processor declaration, extract the processor.
newVar :: ProcDecl -> Proc
newVar (SubTop   p) = p
newVar (CreateLessThan p _) = p
