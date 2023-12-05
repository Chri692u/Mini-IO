module Scope where

import Data.Text
import qualified Data.Map as M

type Id = String

-- | 'WorldInfo' represents the state of the world, including the current working directory and environment variables.
data WorldInfo = WorldInfo { 
    cwd :: FilePath  -- ^ The current working directory.
    , env :: Scope Value  -- ^ The environment variables.
    } deriving (Show)


data Value = Value {
        txt :: Text,
        tag :: Maybe String,
        type_ :: Maybe String
    } deriving (Show)

-- | 'Scope' is a mapping from 'Id' to 'a'.
type Scope a = M.Map Id a

extend :: Scope a -> (Id, a) -> Scope a
extend env (x, v) = M.insert x v env

lookup :: Id -> Scope a -> Maybe a
lookup = M.lookup

emptyScope :: Scope a
emptyScope = M.empty

fromList :: [(Id, a)] -> Scope a
fromList = M.fromList

toList :: Scope a -> [(Id, a)]
toList = M.toList