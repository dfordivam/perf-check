{-# LANGUAGE ForeignFunctionInterface #-}
module Main where

import Prelude hiding (sin)

import Foreign.C -- get the C types
import Foreign.Ptr (Ptr,nullPtr)
import Control.Concurrent

import Data.Time.Clock

foreign import ccall safe "runTasks" runTasks :: IO ()
foreign import ccall safe "runTasks_1" runTasks_1 :: IO ()
foreign import ccall safe "runTasks_2" runTasks_2 :: IO ()
foreign import ccall safe "runTasks_3" runTasks_3 :: IO ()
foreign import ccall safe "runTasks_4" runTasks_4 :: IO ()
foreign import ccall safe "generateExecTasks" generateExecTasks :: IO ()
foreign import ccall safe "clearCount" clearCount :: IO ()
foreign import ccall safe "printCount" printCount :: IO ()
foreign import ccall "runTasks_OpenMP" runTasks_OpenMP :: IO ()
foreign import ccall "initArrays" initArrays :: IO ()

main = do
  initArrays
  putStrLn "Single Thread"
  profileTask runTasks
  putStrLn "OpenMP"
  profileTask runTasks_OpenMP
  putStrLn "Haskell"
  profileTask (runTasksForkIO 100)
  putStrLn "Single Thread"
  profileTask runTasks
  putStrLn "Done"

runTasksForkIO :: Int -> IO ()
runTasksForkIO i = do
  generateExecTasks
  m1 <- newEmptyMVar
  m2 <- newEmptyMVar
  m3 <- newEmptyMVar
  m4 <- newEmptyMVar
  forkFinally runTasks_1 ((\x -> putMVar m1 ()))
  forkFinally runTasks_2 ((\x -> putMVar m2 ()))
  forkFinally runTasks_3 ((\x -> putMVar m3 ()))
  forkFinally runTasks_4 ((\x -> putMVar m4 ()))
  _ <- takeMVar m1
  _ <- takeMVar m2
  _ <- takeMVar m3
  _ <- takeMVar m4
  if (i == 1) then return () else (runTasksForkIO (i-1))


profileTask task = do
  clearCount
  startTime <- getCurrentTime
  task
  endTime <- getCurrentTime
  putStrLn (show (diffUTCTime endTime startTime))
  printCount

