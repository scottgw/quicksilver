module Language.QuickSilver.Generate.LLVM.Values where

import Data.Char

import Control.Applicative

import Language.QuickSilver.Generate.LLVM.Simple
import Language.QuickSilver.Generate.LLVM.Types

true, false :: Build ValueRef
true  = constInt <$> int1TypeM <*> pure 1 <*> pure 0
false = constInt <$> int1TypeM <*> pure 0 <*> pure 0

char :: Char -> Build ValueRef
char c = constInt <$> int8TypeM <*> pure (fromIntegral $ ord c) <*> pure 0

int :: Int -> Build ValueRef
int i = constInt <$> int32TypeM <*> pure (fromIntegral i) <*> pure 0

dbl :: Double -> Build ValueRef
dbl d = 
    let toRealFloat = uncurry encodeFloat (decodeFloat d)
    in constReal <$> doubleTypeM <*> pure toRealFloat