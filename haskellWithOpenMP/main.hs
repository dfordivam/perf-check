{-# LANGUAGE ForeignFunctionInterface #-}
module Main where

import Prelude hiding (sin)

import Foreign.C -- get the C types
import Foreign.Ptr (Ptr,nullPtr)

-- pure function
foreign import ccall "mycfun1" c_sin :: CDouble -> CDouble
sin :: Double -> Double
sin d = realToFrac (c_sin (realToFrac d))

-- impure function
--foreign import ccall "mycfun2" c_time :: Ptr a -> IO CTime
--getTime :: IO CTime
--getTime = c_time nullPtr

main = do
  print  (sin 23.0)
  -- print . sin =<< readLn
--  print =<< getTime
