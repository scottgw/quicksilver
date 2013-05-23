{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.QuickSilver.Position 
    (Pos (..)
    ,Line
    ,Column
    ,SourcePos

    ,sourceLine
    ,sourceColumn
    ,sourceName

    ,inheritPos

    ,attachPos
    ,attachPosM
    ,attachEmptyPos
    ,attachPosBefore
    ,attachPosHere
     
    ,takePos
     
    ,position
    ,contents
    ) where

import Control.Monad
import Control.DeepSeq

import Data.DeriveTH
import Data.Binary
import Data.Hashable
import qualified Data.Data as D
import qualified Data.Typeable as T

import qualified GHC.Generics as G


import Text.Parsec
import Text.Parsec.Pos
import Text.Parsec.ByteString

data Pos a = Pos SourcePos a
             deriving (Ord, G.Generic, D.Data, T.Typeable)

instance Hashable a => Hashable (Pos a)

instance Eq a => Eq (Pos a) where
    (==) p1 p2 = contents p1 == contents p2

instance Show a => Show (Pos a) where
    show p = -- show (position p) ++ "> " ++ 
             show (contents p)

instance Functor Pos where
    fmap f (Pos s a) = Pos s (f a)

inheritPos :: (Pos a -> b) -> Pos a -> Pos b
inheritPos f a = attachPos (position a) (f a)

takePos :: Pos a -> b -> Pos b
takePos pa b = attachPos (position pa) b

attachEmptyPos = attachPos (initialPos "<no file name>")

attachPos :: SourcePos -> a -> Pos a
attachPos = Pos

attachPosM :: Monad m => m SourcePos -> m a -> m (Pos a)
attachPosM = liftM2 attachPos

attachPosHere :: a -> Parser (Pos a)
attachPosHere a = flip attachPos a `fmap` getPosition

attachPosBefore :: Parser a -> Parser (Pos a)
attachPosBefore = attachPosM getPosition

position :: Pos a -> SourcePos
position (Pos p _) = p

contents :: Pos a -> a
contents (Pos _ a) = a

instance Binary SourcePos where
  get = return (newPos "filename lost" 0 0)
  put _p = return ()

instance Hashable SourcePos where
    hashWithSalt s pos = hashWithSalt s (show pos)

-- instance Binary SourcePos where
--     get = do (line, col, name) <- get
--              return (newPos name line col)
            
--     put p = put (sourceLine p, sourceColumn p, sourceName p)

$( derive makeBinary ''Pos )

instance NFData SourcePos where
    rnf p = sourceLine p `seq` sourceColumn p `seq` sourceName p `seq` ()

$( derive makeNFData ''Pos )
