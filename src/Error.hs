module Error where

import Data.Text (Text)
import World

data RuntimeError = FailedCd Text
                  | FailedExist Text
                  | FailedRm Text
                  | FailedMkdir Text
                  | FailedRmdir Text
                  | FailedOpen Text
                  | OtherError

instance Show RuntimeError where
    show (FailedCd dir) = "RUNTIME_ERROR: " ++ "Failed to change directory to " ++ show dir
    show (FailedExist name) = "RUNTIME_ERROR: " ++ "Failed to find: " ++ show name
    show (FailedRm name) = "RUNTIME_ERROR: " ++ "Failed to remove file: " ++ show name
    show (FailedMkdir name) = "RUNTIME_ERROR: " ++ "Failed to create directory: " ++ show name
    show (FailedRmdir name) = "RUNTIME_ERROR: " ++ "Failed to remove directory: " ++ show name
    show (FailedOpen name) = "RUNTIME_ERROR: " ++ "Failed to open file: " ++ show name
    show OtherError = "Other error"