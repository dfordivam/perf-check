{-# LANGUAGE ForeignFunctionInterface #-}
module Main where

import Prelude hiding (sin)

import Foreign.C -- get the C types
import Foreign.Ptr (Ptr,nullPtr)

import Data.Time.Clock

foreign import ccall safe "runtask" runtask :: IO ()
foreign import ccall "runTasks_OpenMP" runTasks_OpenMP :: IO ()
foreign import ccall "initTasks" initTasks :: IO ()

main = do
  initTasks
  runTasks
  profileTask runtask
  profileTask runTasks_OpenMP
  putStrLn "Done"

runTasks = runtask

profileTask task = do
  startTime <- getCurrentTime
  task
  endTime <- getCurrentTime
  putStrLn (show (diffUTCTime endTime startTime))

