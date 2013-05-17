module Language.QuickSilver.Generate.LLVM.Values where

import Data.Char

import Control.Applicative

import Language.QuickSilver.Generate.LLVM.Simple
import Language.QuickSilver.Generate.LLVM.Types

true, false :: Build ValueRef
true  = constInt <$> int1TypeM <*> pure 1 <*> pure False
false = constInt <$> int1TypeM <*> pure 0 <*> pure False

char :: Char -> Build ValueRef
char c = constInt <$> int8TypeM <*> pure (fromIntegral $ ord c) <*> pure False

int :: Int -> Build ValueRef
int i = constInt <$> int64TypeM <*> pure (fromIntegral i) <*> pure False

dbl :: Double -> Build ValueRef
dbl d = 
    let toRealFloat = uncurry encodeFloat (decodeFloat d)
    in constReal <$> doubleTypeM <*> pure toRealFloat
