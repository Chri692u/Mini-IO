module Scope where

import Data.Text
import qualified Data.Map as M

type Id = String

-- | 'WorldInfo' represents the state of the world, including the current working directory and environment variables.
data WorldInfo = WorldInfo { 
    cwd :: FilePath  -- ^ The current working directory.
    , env :: Scope  -- ^ The environment variables.
    } deriving (Show)


data Value = Value {
        txt :: Text,
        tag :: Maybe String,
        type_ :: Maybe String
    } deriving (Show)


newtype Scope = Globals { values :: M.Map Id Value } deriving (Show)

emptyScope :: Scope
emptyScope = Globals M.empty

extend :: Scope -> (Id, Value) -> Scope
extend env (x, s) = env { values = M.insert x s (values env) }

remove :: Scope -> Id -> Scope
remove (Globals env) var = Globals (M.delete var env)

extends :: Scope -> [(Id, Value)] -> Scope
extends env xs = env { values = M.union (M.fromList xs) (values env) }

lookup :: Id -> Scope -> Maybe Value
lookup key (Globals tys) = M.lookup key tys

merge :: Scope -> Scope -> Scope
merge (Globals a) (Globals b) = Globals (M.union a b)

singleton :: Id -> Value -> Scope
singleton x y = Globals (M.singleton x y)

keys :: Scope -> [Id]
keys (Globals env) = M.keys env

fromList :: [(Id, Value)] -> Scope
fromList xs = Globals (M.fromList xs)

toList :: Scope -> [(Id, Value)]
toList (Globals env) = M.toList env