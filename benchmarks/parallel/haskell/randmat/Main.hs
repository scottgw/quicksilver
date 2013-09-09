{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import           Control.DeepSeq
import           Control.Parallel.Strategies

import qualified Data.Vector.Unboxed as Unbox
import           Data.Word

import           System.Environment


randmat :: Int -> Int -> Unbox.Vector Int
randmat s n = 
    Unbox.concat (map createRow [0 .. n - 1] `using` parListChunk 128 rseq)
  where
    createRow i = Unbox.unfoldrN n go (fromIntegral i)

    go :: Word32 -> Maybe (Int, Word32)
    go !seed = Just (fromIntegral (nextSeed `rem` 100), nextSeed)
      where !nextSeed = rand seed

    -- createRow i = Unbox.create (fill i)

    -- fill startIndex = MUnbox.new n >>= \ mv -> go1 startIndex mv >> return mv
    --     where
    --       go1 !i mv = go2 (fromIntegral s + fromIntegral i) 0
    --           where
    --             go2 !seed !j =
    --                 do MUnbox.unsafeWrite mv j (fromIntegral (seed `rem` 100))
    --                    if j >= n
    --                    then return ()
    --                    else go2 (rand seed) (j+1)

rand :: Word32 -> Word32
rand s = lcg_a * s + lcg_c
  where
    lcg_a = 1664525
    lcg_c = 1013904223

main =
  do [seed, n] <- (map read) `fmap` getArgs
     let x = randmat seed n
     print (Unbox.length x)
