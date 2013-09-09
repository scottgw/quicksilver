{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import           Control.Monad.Identity

import qualified Data.Array.Repa as Repa
import           Data.Array.Repa (U, D, DIM2, DIM1, (:.)(..), Z(..), (+^))
import qualified Data.Vector.Unboxed as Unbox
import qualified Data.Vector.Unboxed.Mutable as MUnbox

import           System.Environment

outer :: Int
      -> Repa.Array U DIM1 (Int, Int)
      -> (Repa.Array U DIM2 Double, Repa.Array U DIM1 Double)
outer nelts points = runIdentity $
    do let
          matNoMax :: Repa.Array D DIM2 Double
          matNoMax = Repa.traverse points reshp go
               where
                 reshp (Z :. n) = Z :. n :. n
                 go at (Z :. i :. j)
                   | i /= j = let p1 = at (Z :. i)
                                  p2 = at (Z :. j)
                              in distance p1 p2
                   | otherwise = 0

       !maxes <- Repa.foldP max 0 matNoMax
       
       let
           maxMatrix = Repa.backpermuteDft 
                         (Repa.fromFunction (Z :. nelts :. nelts) (const 0))
                         (\ (Z :. i :. j) ->
                           if i /= j
                           then Just (Z :. i)
                           else Nothing)
                         maxes

       vec' <- Repa.computeUnboxedP (Repa.map (distance (0, 0)) points)
       matWithMax' <- Repa.computeUnboxedP (maxMatrix +^ matNoMax)

       return (matWithMax', vec')
    where
      distance :: (Int, Int) -> (Int, Int) -> Double
      distance (!x1, !y1) (!x2, !y2) =
          let 
              sqrtSub :: Int -> Int -> Double
              sqrtSub !a !b = (fromIntegral $ a - b)^(2 :: Int)
          in
            sqrt (sqrtSub x2 x1 + sqrtSub y2 y1)

main =
  do [nelts] <- (map read) `fmap` getArgs

     points <- MUnbox.new (nelts)
     vPoints <- Unbox.unsafeFreeze points

     let
       toRepa :: Unbox.Unbox a => Unbox.Vector a -> Repa.Array U DIM1 a
       toRepa = Repa.fromUnboxed (Z :. nelts)
  
     let (!mat, !vec) = outer nelts (toRepa vPoints)
     print (Repa.extent mat, Repa.extent vec)
