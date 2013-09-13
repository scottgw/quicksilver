{-# LANGUAGE BangPatterns #-}

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
import           Data.Word

import           System.Environment

thresh :: Int -> Int -> Unbox.Vector Int -> IO (Repa.Array U DIM2 Bool)
thresh n thr v =
    do !arrayMax <- Repa.foldAllP max 0 rv
       print arrayMax
       let
         hist' = mkHist (Repa.toUnboxed rv)

         hist :: IntMap Int
         hist = Map.unionWith (+) hist
                (Map.fromList [(i,0) | i <- [0 .. arrayMax]])

         limit = (n * n * thr) `div` 100

         lastNumber :: Int
         lastNumber = either id (const 0)
                      (foldr go (Right 0) [arrayMax, arrayMax - 1 .. 0])
           where
             go !i (Right !s) | s >= limit = Left i
                              | otherwise  = Right (s + hist Map.! i)
             go _  (Left !i) = Left i

         threshArray :: Repa.Array D DIM2 Bool
         threshArray = Repa.map (< lastNumber) rv
       Repa.computeP threshArray -- Repa.deepSeqArray rv threshArray)
 where
    mkHist :: Unbox.Vector Int -> IntMap Int
    mkHist = Unbox.ifoldl' (\ m i v -> Map.insertWith (+) i v m) Map.empty
    
    rv :: Repa.Array U DIM2 Int
    rv = Repa.fromUnboxed (Z :. n :. n) v

main =
  do [n, thr] <- (map read) `fmap` getArgs
     let v = Unbox.replicate (n * n) 0
     -- v <- Unbox.unsafeFreeze mv
     x <- thresh n thr v
     print (Unbox.length $ Repa.toUnboxed x)
