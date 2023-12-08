{-# LANGUAGE DeriveFunctor #-}

module Transformers.EitherT where

import World

newtype WorldMEitherT a = WorldMEitherT { runWorldMEitherT :: World -> Either String (a, World) } deriving Functor

instance Applicative WorldMEitherT where
    pure x = WorldMEitherT $ \w -> Right (x, w)
    (WorldMEitherT mf) <*> (WorldMEitherT mx) = WorldMEitherT $ \w ->
        case mf w of
            Left err -> Left err
            Right (f, w') ->
                case mx w' of
                    Left err -> Left err
                    Right (x, w'') -> Right (f x, w'')

instance Monad WorldMEitherT where
    (WorldMEitherT mx) >>= f = WorldMEitherT $ \w ->
        case mx w of
            Left err -> Left err
            Right (x, w') -> runWorldMEitherT (f x) w'

instance MonadFail WorldMEitherT where
    fail msg = WorldMEitherT $ \_ -> Left msg

liftWorldM :: WorldM a -> WorldMEitherT a
liftWorldM wm = WorldMEitherT $ \w -> let (x, w') = runIO wm w in Right (x, w')

runWorldMTEither :: World -> WorldMEitherT a -> Either String (a, World)
runWorldMTEither w action = runWorldMEitherT action w