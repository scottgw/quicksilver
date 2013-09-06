{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Monad

import System.Environment

-- Condition test
conditionTest :: Int -> Int -> TVar Int -> IO ()
conditionTest numWorkers numElems var =
  void $ mapConcurrently id (prods ++ cons)
  where
    consumerAction = do
      !x <- readTVar var
      check (x `rem` 2 == 0)
      let !x' = x + 1
      writeTVar var x'
    
    producerAction = do
      !x <- readTVar var
      check (x `rem` 2 == 1)
      let !x' = x + 1
      writeTVar var x'

    makeWorkers = replicate numWorkers . replicateM_ numElems
    prods = makeWorkers (atomically producerAction)
    cons = makeWorkers (atomically consumerAction)


main :: IO ()
main = do
  args <- getArgs
  case args of
    [numElemsStr, numWorkersStr] ->
      newTVarIO 0 >>= conditionTest (read numWorkersStr) (read numElemsStr)
    _ -> putStrLn "Expecting only 2 arguments"