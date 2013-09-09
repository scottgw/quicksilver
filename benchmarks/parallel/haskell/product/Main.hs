{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import           Control.Monad.Identity

import qualified Data.Array.Repa as Repa
import qualified Data.Array.Repa.Unsafe as Repa
import           Data.Array.Repa (U, D, DIM2, DIM1,
                                  (:.)(..), Z(..), (+^), (*^),
                                  All(..))
import qualified Data.Vector.Unboxed as Unbox
import qualified Data.Vector.Unboxed.Mutable as MUnbox

import           System.Environment

prodct :: Repa.Array U DIM2 Double
       -> Repa.Array U DIM1 Double
       -> Repa.Array U DIM1 Double
prodct matrix vector =
  runIdentity $
     let (Z :. n :. _) = Repa.extent matrix
         vectorEx = Repa.unsafeBackpermute
                      (Z :. n :. n)
                      ( \ (Z :. i :. j) -> (Z :. i))
                      vector
     in Repa.sumP (vectorEx *^ matrix)

main =
  do [nelts :: Int] <- (map read) `fmap` getArgs

     vector <- MUnbox.new nelts
     vVector :: Unbox.Vector Double <- Unbox.unsafeFreeze vector

     matrix <- MUnbox.new (nelts * nelts)
     vMatrix :: Unbox.Vector Double <- Unbox.unsafeFreeze matrix

     
     let res = prodct (Repa.fromUnboxed (Z :. nelts :. nelts) vMatrix)
                       (Repa.fromUnboxed (Z :. nelts) vVector)
     print (Repa.extent res)
