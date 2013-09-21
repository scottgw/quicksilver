{-# LANGUAGE BangPatterns #-}

module Main where

import           Control.DeepSeq
import           Control.Parallel
import           Control.Monad.Identity
import           Control.Monad.ST

import qualified Data.Array.Repa as Repa
import           Data.Array.Repa (U, D, DIM2, (:.)(..), Z(..))
import qualified Data.IntMap as Map
import           Data.IntMap (IntMap)
import qualified Data.Vector.Unboxed as Unbox
import qualified Data.Vector.Unboxed.Mutable as MUnbox
import           Data.Word

import           System.Environment

thresh :: Int -> Int -> Unbox.Vector Int -> IO (Repa.Array U DIM2 Bool)
thresh n thr !v =
    do !rv <- return (Repa.fromUnboxed (Z :. n :. n) v)
       !arrayMax <- Repa.deepSeqArray rv (Repa.foldAllP max 0 rv)
       print arrayMax
       let
         limit = (n * n * thr) `div` 100

         lastNumber :: Int
         !lastNumber = either id (const 0)
                       (foldr go (Right 0) [arrayMax, arrayMax - 1 .. 0])
           where
             go !i (Right !s) | s >= limit = Left i
                              | otherwise  = Right (s + hist Map.! i)
             go _  (Left !i) = Left i

         threshArray :: Repa.Array D DIM2 Bool
         threshArray = Repa.map (< lastNumber) rv
       
       !res <- Repa.computeP threshArray
       putStrLn "Foo"
       return res
 where
    hist :: IntMap Int
    hist = Map.fromList $ zip [0..99] (Unbox.toList $ Unbox.create mkHistMVec)
      where
        mkHistMVec :: ST s (Unbox.MVector s Int)
        mkHistMVec =
          do mvec <- MUnbox.replicate 100 0
             let go 0 = return ()
                 go !i =
                   do let !vecVal = Unbox.unsafeIndex v i
                      !count <- MUnbox.unsafeRead mvec vecVal
                      MUnbox.unsafeWrite mvec vecVal (count + 1)
                      go (i-1)
             go (Unbox.length v)
             return mvec

main =
  do [n, thr] <- (map read) `fmap` getArgs
     let v = Unbox.replicate (n * n) 0
     !x <- thresh n thr v
     print (x Repa.! (Z :. 42 :. 42))
