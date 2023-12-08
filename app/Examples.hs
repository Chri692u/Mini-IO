{-# LANGUAGE OverloadedStrings #-}

module Main where

import World
import Core
import Types
import Utility
import Scope
import Control.Monad
import Data.Text (Text, pack)
import Foreign
import Error

helloWorld :: MiniIO ()
helloWorld = printStrM "Hello" >> printStrM "World"

askName :: MiniIO Text
askName = do
    printStrM "What is your name?"
    readStrM

readTxt :: Text -> MiniIO Text
readTxt name = do
    val <- readFileM name
    case val of
        Left err -> return $ pack $ show err
        Right txt -> return txt

askFile :: MiniIO Text
askFile = do
    printStrM "What file do you want to read?"
    filename <- readStrM
    readTxt filename

navigation :: MiniIO ()
navigation = do
    printStrM "cwd"
    cwd <- getCwdM
    printStrM cwd
    printStrM "-- ls"
    dirs <- listDirectoryM
    forM_ dirs printStrM
    printStrM "ls end --"
    printStrM "cd src"
    result <- changeDirectoryM "src"
    case result of
        Left err -> printStrM $ pack $ show err
        Right () -> do
            printStrM "Changed directory"


makedir :: MiniIO ()
makedir = do
    printStrM "mkdir test"
    result <- createDirectoryM "test"
    case result of
        Left err -> printStrM $ pack $ show err
        Right () -> printStrM "Created directory"

rmdir :: MiniIO ()
rmdir = do
    printStrM "rmdir test"
    result <- removeDirectoryM "test"
    case result of
        Left err -> printStrM $ pack $ show err
        Right () -> printStrM "Removed directory"

initWorld :: MiniIO Text
initWorld = do
    dir <- getCwdM
    declareVarM "test" (Value (pack "test") Nothing Nothing)
    return dir

askFile' :: MiniIO ()
askFile' = (printStrM "What file do you want to read?" >> readStrM) |> readTxt |> printStrM

usePass :: MiniIO ()
usePass = pass $ return "Test String"

readmeSize :: MiniIO ()
readmeSize = do
    v <- getFileSizeM "README.md"
    printStrM $ pack $ show v

dirSize :: MiniIO ()
dirSize = do
    v <- getDirectorySizeM "src"
    printStrM $ pack $ show v

testfiles :: MiniIO ()
testfiles = do
    createFileM "test.txt"
    handle <- openFileM "test.txt"
    case handle of
        Left err -> printStrM $ pack $ show err
        Right ptr -> do
            appendToFileM ptr "Hello World"
            res <- closeFileM ptr
            case res of
                Left err -> printStrM $ pack $ show err
                Right () -> printStrM "Closed file"

main :: IO ()
main = do 
    let iw = World (WorldInfo "not-set" emptyScope)
    let w = execIO initWorld iw
    let res = runIO helloWorld w
    print res