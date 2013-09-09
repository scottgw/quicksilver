{-# LANGUAGE BangPatterns #-}

module Main where

import           Control.Concurrent.Async
import           Control.DeepSeq
import           Control.Monad.Identity
import           Control.Parallel

import qualified Data.Array.Repa as Repa
import           Data.Array.Repa (U, D, DIM1, DIM2, (:.)(..), Z(..))
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as Unbox (unsafeFreeze)
import qualified Data.Vector.Unboxed.Mutable as Unbox
import           Data.Vector.Strategies
import           Data.Word

import           System.Environment

-- randmat :: Int -> Int -> Repa.Array U DIM2 Int
-- randmat s n = runIdentity $
--   do aa <- Repa.computeP (Repa.map createRow a)
--      Repa.computeP (flattened aa)
--   where
--     arr :: Repa.Array U DIM2 Int
--     arr = Repa.fromUnboxed (Z :. n :. 1) (Unbox.generate n (+s))

--     step a = Repa.traverse a (\ (Z :. n :. i) -> Z :. n :. i + 1) go
--       where
--         go at (Z :. 

--     flattened :: Repa.Array D DIM1 (Unbox.Vector Int) -> Repa.Array D DIM2 Int
--     flattened v = Repa.traverse v (\ (Z :. n) -> Z :. n :. n) lookupInVec
--       where
--         lookupInVec at (Z :. i :. j) = Unbox.unsafeIndex (at (Z :. i)) j

--     createRow :: Int -> Unbox.Vector Int
--     createRow i = Unbox.unfoldrN n go (fromIntegral i)

--     go :: Word32 -> Maybe (Int, Word32)
--     go !seed = Just (fromIntegral (nextSeed `rem` 100), nextSeed)
--       where !nextSeed = rand seed


randmat :: Int -> Int -> IO (Repa.Array U DIM2 Int)
randmat s n = 
  do mv <- Unbox.new (n * n)
     let w = 32 -- Number of workers

         height | w > n = 1
                | otherwise = n `div` w

         startIndices | w > n     = [0 .. n - 1]
                      | otherwise = [i*height | i <- [0.. w - 1]]

         fill startIndex = mapM_ go1 [startIndex .. startIndex + height - 1]
           where
             go1 !i = go2 (fromIntegral s + fromIntegral i) 0
               where
                 go2 :: Word32 -> Int -> IO ()
                 go2 !seed !j =
                   do Unbox.unsafeWrite mv (n * i + j) (fromIntegral (seed `rem` 100))
                      if j >= n
                        then return ()
                        else go2 (rand seed) (j+1)

     mapConcurrently fill startIndices
     v <- Unbox.unsafeFreeze mv
     return (Repa.fromUnboxed (Z :. n :. n) v)

rand :: Word32 -> Word32
rand s = lcg_a * s + lcg_c
  where
    lcg_a = 1664525
    lcg_c = 1013904223

main =
  do [seed, n] <- (map read) `fmap` getArgs
     x <- randmat seed n
     print (Repa.extent $ Repa.deepSeqArray x x)
