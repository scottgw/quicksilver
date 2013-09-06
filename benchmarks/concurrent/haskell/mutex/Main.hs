{-# LANGUAGE BangPatterns #-}
module Main where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad

import System.Environment

main :: IO ()
main = do
  args <- getArgs
  case args of
    [numWorkersStr, numElemsStr] ->
      newTVarIO 0 >>= mutexTest (read numWorkersStr) (read numElemsStr)
    _ -> putStrLn "Expecting only 2 arguments"
    

-- Mutex test
mutexTest :: Int -> Int -> TVar Int -> IO ()
mutexTest numWorkers numElems var = 
  void $ mapConcurrently id muts >> (atomically (readTVar var) >>= print)
  where
    muts = replicate numWorkers mutexLoop
    
    mutexIteration = do
      !x <- readTVar var
      let !x' = x + 1
      writeTVar var x'

    mutexLoop = atomically $ replicateM_ numElems mutexIteration
