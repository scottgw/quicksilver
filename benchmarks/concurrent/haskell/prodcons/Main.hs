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
      newTQueueIO >>=  prodConsTest (read numWorkersStr) (read numElemsStr)
    _ -> putStrLn "Expecting only 2 arguments"


-- N-M producer consumer test
prodConsTest :: Int -> Int -> TQueue () -> IO ()
prodConsTest numWorkers numElems conduit =
  let prods = replicate numWorkers (atomically producer)
      cons = replicate numWorkers (atomically consumer)
      
      consumer = replicateM_ numElems (readTQueue conduit)
      producer = replicateM_ numElems (writeTQueue conduit ())
  in void $ mapConcurrently id (prods ++ cons)
