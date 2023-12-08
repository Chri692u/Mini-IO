{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE BangPatterns #-}

module Jobs.Unix where

import World
import Types
import Utility
import Data.Text (Text, pack, unpack)
import Foreign.C
import System.IO.Unsafe ( unsafePerformIO )

--------------------------------------------------------------------------------
-- Foreign Function Interface (unholy zone)
--------------------------------------------------------------------------------

foreign import ccall "c_fork" c_fork :: IO CInt

fork :: World -> Result (Maybe Int)
fork !w = unsafePerformIO $ do
           pid <- c_fork
           if pid == 0 then
               return (Nothing, w)
           else
               return (Just $ fromIntegral pid, w)

--------------------------------------------------------------------------------
-- MiniIO monad wrappers
--------------------------------------------------------------------------------
forkT :: WorldT (Maybe Int)
forkT = fork

forkM :: WorldM (Maybe Int)
forkM = WorldM forkT