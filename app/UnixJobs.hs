module Main where

import World
import Core
import Jobs.Unix
import Types
import Utility
import Scope
import Error

import Data.Text (Text, pack, unpack)
import Foreign.C.String

initWorld :: MiniIO Text
initWorld = getCwdM

forkPrint :: MiniIO ()
forkPrint = do
    pid <- forkM
    case pid of
        Just pid'  -> do
            if pid' == 0 then
                printStrM $ pack "I am the child"
            else
                printStrM $ pack "I am the parent"

        _ -> error "error"

main :: IO ()
main = do 
    let iw = World (WorldInfo "not-set" emptyScope)
    let iw' = execIO initWorld iw
    -- use of ForkM (not tested yet)
    let w = execIO forkPrint iw'
    print w