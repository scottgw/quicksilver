{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import           Control.Monad.Identity

import qualified Data.Array.Repa as Repa
import           Data.Array.Repa (U, D, DIM2, DIM1, (:.)(..), Z(..))
import qualified Data.IntMap as Map
import           Data.IntMap (IntMap)
import qualified Data.Vector.Unboxed as Unbox
import qualified Data.Vector.Unboxed.Mutable as MUnbox
import qualified Data.Vector.Algorithms.Intro as Intro

import           System.Environment

bool2Int :: Bool -> Int
bool2Int b = if b then 1 else 0

{-# INLINE (!) #-}
(!) :: Unbox.Unbox e => Repa.Array U DIM2 e -> Int -> e
(!) = Repa.unsafeLinearIndex

winnow :: Int
       -> Int
       -> Repa.Array U DIM2 Int
       -> Repa.Array U DIM2 Bool
       -> Unbox.Vector (Int, Int)
winnow n nelts matrix mask = runIdentity $
    do rough <- roughPts
       let sorted = sortPts (Repa.toUnboxed rough)
       sel <- selectedPts sorted
       return (Repa.toUnboxed sel)
  where
    selectedPts x = Repa.selectP sel mkPt (Unbox.length x)
        where
          chunk = Unbox.length x `div` nelts
          sel i   = i `rem` chunk == 0
          mkPt !idx = let (_, i, j) = Unbox.unsafeIndex x idx
                      in (i, j)

    -- Sorting the selected unmasked points
    sortPts = Unbox.modify Intro.sort

    -- Select the unmasked points from the matrix
    -- roughPts :: Identity (Repa.Array U DIM1 (Int, (Int, Int)))
    roughPts = Repa.selectP sel mkPt (n*n)
        where
          sel !x = let (i, j) = idx2d n x
                   in ((i * j) `rem` (n + 1)) == 1
          -- mask ! i
          mkPt !idx = let (i, j) = idx2d n idx
                      in (matrix ! idx, i, j)

    idx2d :: Int -> Int -> (Int, Int)
    idx2d !n !i = i `quotRem` n

main =
  do [n, nelts] <- (map read) `fmap` getArgs

     mask <- MUnbox.new (n * n)
     vMask <- Unbox.unsafeFreeze mask


     matrix <- MUnbox.new (n * n)
     MUnbox.set matrix 0
     vMatrix <- Unbox.unsafeFreeze matrix
     -- vMatrix <-  Unbox.replicate (n * n) 0

     let
       toRepa :: Unbox.Unbox a => Unbox.Vector a -> Repa.Array U DIM2 a
       toRepa = Repa.fromUnboxed (Z :. n :. n)
  
     let x = winnow n nelts (toRepa vMatrix) (toRepa vMask)
     print (Unbox.length x)
