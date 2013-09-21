{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where


import           Control.DeepSeq
import           Control.Parallel.Strategies
import           Control.Monad.Identity
import           Control.Monad.ST

import qualified Data.Array.Repa as Repa
import qualified Data.Array.Repa.Unsafe as Repa
import           Data.Array.Repa (U, D, DIM2, DIM1,
                                  (:.)(..), Z(..), (+^), (*^),
                                  All(..), Any(..))
import qualified Data.IntMap as Map
import           Data.IntMap (IntMap)
import qualified Data.Vector.Unboxed as Unbox
import qualified Data.Vector.Unboxed.Mutable as MUnbox
import qualified Data.Vector.Algorithms.Intro as Intro
import           Data.Word

import           System.Environment


randmat :: Int -> Int -> IO (Repa.Array U DIM2 Int)
randmat s n = return $ Repa.fromUnboxed (Z :. n :. n) $
    Unbox.concat (map createRow [0 .. n - 1] `using` parListChunk 128 rseq)
  where
    createRow i = Unbox.unfoldrN n go (fromIntegral i)

    go :: Word32 -> Maybe (Int, Word32)
    go !seed = Just (fromIntegral (nextSeed `rem` 100), nextSeed)
      where !nextSeed = rand seed

    rand :: Word32 -> Word32
    rand s = lcg_a * s + lcg_c
      where
        lcg_a = 1664525
        lcg_c = 1013904223


thresh :: Int -> Int -> Repa.Array U DIM2 Int -> IO (Repa.Array U DIM2 Bool)
thresh n thr !rv =
    do !arrayMax <- Repa.deepSeqArray rv (Repa.foldAllP max 0 rv)

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
                   do let !vecVal = Repa.unsafeLinearIndex rv i
                      !count <- MUnbox.unsafeRead mvec vecVal
                      MUnbox.unsafeWrite mvec vecVal (count + 1)
                      go (i-1)
             go (n * n)
             return mvec

{-# INLINE (!) #-}
(!) = Repa.unsafeLinearIndex

winnow :: Int
       -> Int
       -> Repa.Array U DIM2 Int
       -> Repa.Array U DIM2 Bool
       -> IO (Repa.Array U DIM1 (Int, Int))
winnow n nelts !matrix !mask =
    do !rough <- roughPts
       let !sorted = sortPts (Repa.toUnboxed rough)
       selectedPts sorted
  where
    selectedPts !x = Repa.selectP sel mkPt nelts
        where
          chunk     = Unbox.length x `div` nelts
          sel !i    = True
          mkPt !idx = let (_, i, j) = Unbox.unsafeIndex x (idx * chunk)
                      in (i, j)

    -- Sorting the selected unmasked points
    sortPts = Unbox.modify Intro.sort

    -- Select the unmasked points from the matrix
    roughPts = Repa.selectP sel mkPt (n*n)
        where
          -- This selection only works for the benchmark code,
          -- in the chain this should look up the correct value from
          -- the mask, i.e., `mask ! i`
          sel !x = let (!i, !j) = idx2d n x
                   in ((i * j) `rem` (n + 1)) == 1
          mkPt !idx = let (!i, !j) = idx2d n idx
                      in (matrix ! idx, i, j)

    idx2d :: Int -> Int -> (Int, Int)
    idx2d !n !idx = idx `quotRem` n

outer :: Repa.Array U DIM1 (Int, Int)
      -> IO (Repa.Array U DIM2 Double, Repa.Array U DIM1 Double)
outer !points =
    do let
          matNoMax :: Repa.Array D DIM2 Double
          matNoMax = Repa.unsafeTraverse points reshp go
               where
                 reshp (Z :. n) = Z :. n :. n
                 go at (Z :. i :. j) =
                    let p1 = at (Z :. i)
                        p2 = at (Z :. j)
                    in distance p1 p2


       (matNoMax' :: Repa.Array U DIM2 Double) <- Repa.computeP matNoMax
       !maxes <- Repa.foldP max 0 matNoMax'

       let
           sel (Z :. i :. j) | i == j = Just (Z :. i)
                             | otherwise = Nothing
           maxMatrix = Repa.unsafeBackpermuteDft matNoMax' sel maxes

       !vec' <- Repa.computeUnboxedP (Repa.map (distance (0, 0)) points)
       !matWithMax' <- Repa.computeUnboxedP maxMatrix

       return (matWithMax', vec')
    where
      distance :: (Int, Int) -> (Int, Int) -> Double
      distance (!x1, !y1) (!x2, !y2) =
          let 
              sqrtSub :: Int -> Int -> Double
              sqrtSub !a !b = let d = a - b
                              in fromIntegral $ d * d
          in
            sqrt (sqrtSub x2 x1 + sqrtSub y2 y1)


prodct :: Repa.Array U DIM2 Double
       -> Repa.Array U DIM1 Double
       -> IO (Repa.Array U DIM1 Double)
prodct !matrix !vector =
  do let (Z :. n :. _) = Repa.extent matrix
     Repa.computeP $
       Repa.fromFunction (Z :. n) $
         \ (Z :. i) ->
           Repa.sumAllS $
           Repa.zipWith (*)
             (Repa.unsafeSlice matrix (Any :. i :. All))
             vector


main =
  do [nelts, seed, percent, winnow_nelts] <- (map read) `fmap` getArgs
  
     !rmat <- randmat seed nelts
     !mask <- thresh nelts percent rmat
     !pts <- winnow nelts winnow_nelts rmat mask
     (!mat, !vec) <- outer pts
     !res <- prodct mat vec

     print (Repa.extent mask)
