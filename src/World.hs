{-# LANGUAGE DeriveFunctor #-}

module World where

import Scope
import Control.Monad.Fail (MonadFail, fail)
-- | 'World' is a newtype wrapper for 'WorldInfo'.
newtype World = World { wi :: WorldInfo } deriving (Show)

-- | 'WorldT' is a state transformer for 'World'. It takes a 'World' and returns a value and a new 'World'.
type WorldT a = World -> (a, World)

-- | 'WorldM' is a monad for 'WorldT'. It wraps a 'WorldT' computation in a newtype.
newtype WorldM a = WorldM { runIO :: WorldT a } deriving Functor

-- | 'bind'' is a helper function for chaining 'WorldT' computations together.
bind' :: WorldT a -> (a -> WorldT b) -> WorldT b
bind' wt f w = let (x, w') = wt w in f x w'

-- | The 'Applicative' instance for 'WorldM' allows you to apply a function wrapped in a 'WorldM' to a value wrapped in a 'WorldM'.
instance Applicative WorldM where
    pure x = WorldM $ \w -> (x, w)
    wf <*> wt = WorldM (runIO wf `bind'` \f ->
        runIO wt `bind'` \x ->  runIO $ pure $ f x)

-- | The 'Monad' instance for 'WorldM' allows you to chain 'WorldM' computations together using the '>>=' operator.
instance Monad WorldM where
    wt >>= f = WorldM (runIO wt `bind'` (runIO . f))

instance MonadFail WorldM where
    fail msg = WorldM $ \_ -> error msg