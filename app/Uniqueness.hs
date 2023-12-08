module Main where

import World
import Core
import Types
import Utility
import Scope
import Error

import Data.Text (Text, pack, unpack)

initWorld :: MiniIO Text
initWorld = getCwdM

useCd :: Text -> MiniIO ()
useCd txt = do
    result <- changeDirectoryM txt
    case result of
        Left err -> printStrM $ pack $ show err
        Right () -> do
            return ()

cdTwice :: Text -> MiniIO ()
cdTwice txt = do
    result <- changeDirectoryM txt -- change in world
    case result of
        Left err -> printStrM $ pack $ show err
        Right () -> do
            result' <- changeDirectoryM txt -- change in world
            case result' of
                Left err -> printStrM $ pack $ show err
                Right () -> do
                    return ()
main :: IO ()
main = do 
    let iw = World (WorldInfo "not-set" emptyScope)
    let iw' = execIO initWorld iw
    -- We should not use the same world for two different computations
    let (val, w) = runIO (useCd $ pack "src") iw'
    let (val', w') = runIO (useCd $ pack "ffi") w
    -- the MiniIO monad handles world automatically
    let (val'', w'') = runIO (cdTwice $ pack "..") w'
    print w''