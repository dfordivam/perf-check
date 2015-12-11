{-# LANGUAGE ForeignFunctionInterface #-}
module Main where

import Prelude hiding (sin)

import Foreign.C -- get the C types
import Foreign.Ptr (Ptr,nullPtr)
import Foreign.StablePtr
import Control.Concurrent.MVar
import Control.Concurrent
import Data.IORef
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

-- Store pointer on c side
foreign import ccall "initArrays" initArrays :: StablePtr GlobalData -> IO ()

foreign export ccall addToGlobal :: StablePtr GlobalData -> CInt -> IO ()
foreign export ccall addToGlobalLocked :: StablePtr GlobalData -> CInt -> IO ()

main = do
  gd <- doInit
  putStrLn "Single Thread"
  profileTask gd runTasks
  putStrLn "OpenMP"
  profileTask gd runTasks_OpenMP
  putStrLn "Haskell"
  profileTask gd (runTasksForkIO 1000)
  putStrLn "Single Thread"
  profileTask gd runTasks
  putStrLn "Done"

doInit = do
  i1 <- newIORef 0
  i2 <- newIORef 0
  m <- newEmptyMVar
  let gd = GlobalData i1 i2 m
  sptr <- newStablePtr gd
  initArrays sptr
  return gd

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


profileTask gd task = do
  clearCount
  modifyIORef (lockedInt gd) (\_ -> 0)
  startTime <- getCurrentTime
  task
  endTime <- getCurrentTime
  putStrLn (show (diffUTCTime endTime startTime))
  printCount
  count <- readIORef (lockedInt gd)
  putStrLn ("Locked count : " ++ show count)

data GlobalData = GlobalData {
    unlockedInt :: IORef CInt
  , lockedInt   :: IORef CInt
  , lockedIntMVar   :: MVar CInt
  }

addToGlobalLocked :: StablePtr GlobalData -> CInt -> IO ()
addToGlobalLocked sptr i = do
  gd <- deRefStablePtr sptr
  --modifyIORef (lockedInt gd) (\x -> (x + 1))
  atomicModifyIORef (lockedInt gd) (\x -> (x + 1, ()))

addToGlobal :: StablePtr GlobalData -> CInt -> IO ()
addToGlobal sptr i = do
  gd <- deRefStablePtr sptr
  modifyIORef (unlockedInt gd) (+ 1)
