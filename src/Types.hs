module Types where

import Data.Text (Text)
import Foreign (Ptr)
import World
import Transformers.EitherT
import Error

type MiniIO = WorldM
type MiniIOE = WorldMEitherT
type Ref = Ptr ()
type Result a = (a, World) 
type RuntimeResult a = Either RuntimeError (Result a)
type RuntimeEffect =  Either RuntimeError World