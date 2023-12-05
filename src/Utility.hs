module Utility where

import World
import Data.Text (Text, pack, unpack)
import Control.Monad

-- | 'MiniIO' is a type alias for 'WorldM'.
type MiniIO = WorldM

-- | 'pipe' is a monadic function composition.
pipe :: Monad m => m t -> (t -> m b) -> m b
pipe m1 m2 = do
    val <- m1
    m2 val

-- | 'pipe' operator
(|>) :: Monad m => m t -> (t -> m b) -> m b
(|>) = pipe

-- | 'pipe' operator with flipped arguments
(<|) :: Monad m => (t -> m b) -> m t -> m b
(<|) = flip pipe


-- | 'pass' discards the result of a monadic computation.
pass :: Monad m => m t -> m ()
pass = void

-- | 'evalIO' runs an 'MiniIO' computation and returns only the result.
evalIO :: MiniIO a -> World -> a
evalIO f w = fst $ runIO f w

-- | 'execIO' runs an 'MiniIO' computation and returns only the new 'World'.
execIO :: MiniIO a -> World -> World
execIO f w = snd $ runIO f w