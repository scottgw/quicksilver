{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.DeepSeq
import           Control.Parallel
import           Control.Monad.Identity

import qualified Data.Array.Repa as Repa
import           Data.Array.Repa (U, D, DIM2, (:.)(..), Z(..))
import qualified Data.IntMap as Map
import           Data.IntMap (IntMap)
import qualified Data.Vector.Unboxed as Unbox
import qualified Data.Vector.Unboxed.Mutable as Unbox (new)
import           Data.Vector.Strategies
import           Data.Word

import           System.Environment

bool2Int :: Bool -> Int
bool2Int b = if b then 1 else 0

winnow :: Int
       -> Int
       -> Repa.Array U DIM2 Int
       -> Repa.Array U DIM2 Bool
       -> IO (Unbox.Vector (Int, Int))
winnow n nelts matrix mask =
  do 
     return undefined

  where
    win :: Repa.Array U DIM2 Int
        -> Repa.Array U DIM2 Bool
        -> IO (Int, Unbox.Vector Int)
    win matrix mask =
      do s <- Repa.sumAllP (Repa.map bool2Int mask)
         let v = Unbox.create 
main =
  do [n, nelts] <- (map read) `fmap` getArgs

     mvMask <- Unbox.new (n * n)
     vMask <- Unbox.unsafeFreeze mvMask

     mvMatrix <- Unbox.new (n * n)
     vMatrix <- Unbox.unsafeFreeze mvMatrix

     let
       toRepa :: Unbox.Unbox a => Unbox.Vector a -> Repa.Array U DIM2 a
       toRepa = Repa.fromUnboxed (Z :. n :. n)
  
     x <- winnow n nelts (toRepa vMatrix) (toRepa vMask)
     print (Unbox.length x)
