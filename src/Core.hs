{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Core where
import World
import Scope
import Error

import Foreign.C
import Foreign
import Data.Text (Text, pack, unpack)
import System.IO.Unsafe ( unsafePerformIO )


--------------------------------------------------------------------------------
-- Foreign Function Interface (unholy zone)
--------------------------------------------------------------------------------

foreign import ccall "printStr" c_printStr :: CString -> IO ()
foreign import ccall "readStr" c_readStr :: CString -> CInt -> IO ()
foreign import ccall "readFile" c_readFile :: CString -> IO (Ptr CChar)
foreign import ccall "currentDirectory" c_cwd :: IO CString
foreign import ccall "listDirectory" c_ls :: CString -> IO (Ptr CString)
foreign import ccall "changeDirectory" c_cd :: CString -> IO CInt
foreign import ccall "createFile" c_touch :: CString -> IO ()
foreign import ccall "removeFile" c_rm :: CString -> IO CInt
foreign import ccall "createDirectory" c_mkdir :: CString -> IO CInt
foreign import ccall "removeDirectory" c_rmdir :: CString -> IO CInt
-- test these 2 later
foreign import ccall "getFileSize" c_sizefile :: CString -> IO CLong
foreign import ccall "getDirectorySize" c_sizedir :: CString -> IO CLong
-- test later please
foreign import ccall "open_file" c_open_file :: CString -> CString -> IO (Ptr ())
foreign import ccall "close_file" c_close_file :: Ptr () -> IO ()
foreign import ccall "append_to_file" c_append_to_file :: Ptr () -> CString -> IO ()

printStr :: Text -> World -> World
printStr str !w = unsafePerformIO $ withCString (unpack str) c_printStr >> return w

readStr :: World -> (Text, World)
readStr !w = unsafePerformIO $ allocaArray 4096 $ \buffer -> do
    c_readStr buffer 4096
    str <- peekCString buffer
    return (pack str, w)

getCwd :: World -> (Text, World)
getCwd !w = unsafePerformIO $ do
    case cwd $ wi w of
        "not-set" -> do
            cdir <- c_cwd
            dir <- peekCString cdir
            free cdir
            return (pack dir, World (WorldInfo dir (env $ wi w)))
        _ -> do
            return (pack (cwd $ wi w), w)

declareVar :: Id -> Value -> World -> World
declareVar name val !w =
    let env' = extend (env $ wi w) (name, val)
    in World (WorldInfo (cwd $ wi w) env')

listDirectory :: World -> ([Text], World)
listDirectory !w = unsafePerformIO $ do
    cdir <- c_cwd
    cdirs <- c_ls cdir
    dirs <- peekArray0 nullPtr cdirs >>= mapM peekCString
    free cdirs
    return (map pack dirs, w)

changeDirectory :: Text -> World -> Either RuntimeError World
changeDirectory dirname !w = unsafePerformIO $ withCString (unpack dirname) $ \cdirname -> do
    result <- c_cd cdirname
    if result == 0 then do
        cdir <- c_cwd
        dir <- peekCString cdir
        free cdir
        let w' = World (WorldInfo dir (env $ wi w))
        return (Right w')
    else
        return (Left $ FailedCd dirname)

createDirectory :: Text -> World -> Either RuntimeError World
createDirectory dirname !w = unsafePerformIO $ withCString (unpack dirname) $ \cdirname -> do
    result <- c_mkdir cdirname
    if result == 0 then
        return (Right w)
    else
        return (Left $ FailedMkdir dirname)

removeDirectory :: Text -> World -> Either RuntimeError World
removeDirectory dirname !w = unsafePerformIO $ withCString (unpack dirname) $ \cdirname -> do
    result <- c_rmdir cdirname
    if result == 0 then
        return (Right w)
    else 
        return (Left $ FailedRmdir dirname)

getFileContent :: Text -> World -> Either RuntimeError (Text, World)
getFileContent filename !w = unsafePerformIO $ withCString (unpack filename) $ \cfilename -> do
    ccontents <- c_readFile cfilename
    if ccontents == nullPtr
        then return (Left $ FailedExist filename)
        else do
            contents <- peekCString ccontents
            free ccontents
            return (Right (pack contents, w))

createFile :: Text -> World -> World
createFile filename !w = unsafePerformIO $ withCString (unpack filename) c_touch >> return w

removeFile :: Text -> World -> Either RuntimeError World
removeFile filename !w = unsafePerformIO $ withCString (unpack filename) $ \cfilename -> do
    result <- c_rm cfilename
    if result == 0 then
        return (Right w)
    else 
        return (Left $ FailedRm $ pack "Failed to remove file")

getFileSize :: Text -> World -> Either RuntimeError (Integer, World)
getFileSize filename !w = unsafePerformIO $ withCString (unpack filename) $ \cfilename -> do
    size <- c_sizefile cfilename
    if size == -1
        then return (Left $ FailedExist filename)
        else return (Right (fromIntegral size, w))

getDirectorySize :: Text -> World -> Either RuntimeError (Integer, World)
getDirectorySize dirname !w = unsafePerformIO $ withCString (unpack dirname) $ \cdirname -> do
    size <- c_sizedir cdirname
    if size == -1
        then return (Left $ FailedExist dirname)
        else return (Right (fromIntegral size, w))

openFile :: Text -> World -> Either RuntimeError (Ptr (), World)
openFile filename !w = unsafePerformIO $ withCString (unpack filename) $ \cfilename -> 
    withCString "r+" $ \cmode -> do
        handler <- c_open_file cfilename cmode
        if handler == nullPtr
            then return (Left $ FailedOpen filename)
            else return (Right (handler, w))

closeFile :: Ptr () -> World -> Either RuntimeError World
closeFile handler !w = unsafePerformIO $ do
    c_close_file handler
    return (Right w)

appendToFile :: Ptr () -> Text -> World -> Either RuntimeError World
appendToFile handler content !w = unsafePerformIO $ withCString (unpack content) $ \ccontent -> do
    c_append_to_file handler ccontent
    return (Right w)

--------------------------------------------------------------------------------
-- Core IO functions
--------------------------------------------------------------------------------

readStrT :: WorldT Text
readStrT = readStr

printStrT :: Text -> WorldT ()
printStrT s w = ((), printStr s w)

readFileT :: Text -> WorldT (Either RuntimeError Text)
readFileT filename w = case getFileContent filename w of
    Left errMsg -> (Left errMsg, w)
    Right (contents, newWorld) -> (Right contents, newWorld)

createFileT :: Text -> WorldT ()
createFileT filename w = ((), createFile filename w)

removeFileT :: Text -> WorldT (Either RuntimeError ())
removeFileT filename w = case removeFile filename w of
    Left errMsg -> (Left errMsg, w)
    Right newWorld -> (Right (), newWorld)

getCwdT :: WorldT Text
getCwdT = getCwd

listDirectoryT :: World -> ([Text], World)
listDirectoryT = listDirectory

changeDirectoryT :: Text -> WorldT (Either RuntimeError ())
changeDirectoryT dirname w = case changeDirectory dirname w of
    Left errMsg -> (Left errMsg, w)
    Right newWorld -> (Right (), newWorld)

createDirectoryT :: Text -> WorldT (Either RuntimeError ())
createDirectoryT dirname w = case createDirectory dirname w of
    Left errMsg -> (Left errMsg, w)
    Right newWorld -> (Right (), newWorld)

removeDirectoryT :: Text -> WorldT (Either RuntimeError ())
removeDirectoryT dirname w = case removeDirectory dirname w of
    Left errMsg -> (Left errMsg, w)
    Right newWorld -> (Right (), newWorld)


declareVarT :: Id -> Value -> WorldT ()
declareVarT name val w = ((), declareVar name val w)

getFileSizeT :: Text -> WorldT (Either RuntimeError Integer)
getFileSizeT filename w = case getFileSize filename w of
    Left errMsg -> (Left errMsg, w)
    Right (size, newWorld) -> (Right size, newWorld)

getDirectorySizeT :: Text -> WorldT (Either RuntimeError Integer)
getDirectorySizeT dirname w = case getDirectorySize dirname w of
    Left errMsg -> (Left errMsg, w)
    Right (size, newWorld) -> (Right size, newWorld)

openFileT :: Text -> WorldT (Either RuntimeError (Ptr ()))
openFileT filename w = case openFile filename w of
    Left errMsg -> (Left errMsg, w)
    Right (handler, newWorld) -> (Right handler, newWorld)

closeFileT :: Ptr () -> WorldT (Either RuntimeError ())
closeFileT handler w = case closeFile handler w of
    Left errMsg -> (Left errMsg, w)
    Right newWorld -> (Right (), newWorld)

appendToFileT :: Ptr () -> Text -> WorldT (Either RuntimeError ())
appendToFileT handler content w = case appendToFile handler content w of
    Left errMsg -> (Left errMsg, w)
    Right newWorld -> (Right (), newWorld)

-- | 'readStrM' reads a string from the user.
readStrM :: WorldM Text
readStrM = WorldM readStrT

-- | 'printStrM' prints a string.
printStrM :: Text -> WorldM ()
printStrM = WorldM . printStrT

-- | 'readFileM' reads the contents of a file.
readFileM :: Text -> WorldM (Either RuntimeError Text)
readFileM = WorldM . readFileT

-- | 'createFileM' creates a file.
createFileM :: Text -> WorldM ()
createFileM filename = WorldM $ createFileT filename

-- | 'removeFileM' removes a file.
removeFileM :: Text -> WorldM (Either RuntimeError ())
removeFileM = WorldM . removeFileT

-- | 'getCwdM' gets the current working directory.
getCwdM :: WorldM Text
getCwdM = WorldM getCwdT

-- | 'listDirectoryM' lists the contents of a directory.
listDirectoryM :: WorldM [Text]
listDirectoryM = WorldM listDirectoryT

-- | 'changeDirectoryM' changes the current working directory.
changeDirectoryM :: Text -> WorldM (Either RuntimeError ())
changeDirectoryM = WorldM . changeDirectoryT

-- | 'createDirectoryM' creates a directory.
createDirectoryM :: Text -> WorldM (Either RuntimeError ())
createDirectoryM = WorldM . createDirectoryT

-- | 'removeDirectoryM' removes a directory.
removeDirectoryM :: Text -> WorldM (Either RuntimeError ())
removeDirectoryM = WorldM . removeDirectoryT

-- | 'declareVarM' declares a variable in the environment.
declareVarM :: Id -> Value -> WorldM ()
declareVarM name val = WorldM $ declareVarT name val

-- | 'getFileSizeM' gets the size of a file.
getFileSizeM :: Text -> WorldM (Either RuntimeError Integer)
getFileSizeM = WorldM . getFileSizeT

-- | 'getDirectorySizeM' gets the size of a directory.
getDirectorySizeM :: Text -> WorldM (Either RuntimeError Integer)
getDirectorySizeM = WorldM . getDirectorySizeT

-- | 'openFileM' opens a file.
openFileM :: Text -> WorldM (Either RuntimeError (Ptr ()))
openFileM = WorldM . openFileT

-- | 'closeFileM' closes a file.
closeFileM :: Ptr () -> WorldM (Either RuntimeError ())
closeFileM handler = WorldM $ closeFileT handler

-- | 'appendToFileM' appends to a file.
appendToFileM :: Ptr () -> Text -> WorldM (Either RuntimeError ())
appendToFileM handler content = WorldM $ appendToFileT handler content