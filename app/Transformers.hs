module Main where

import World
import Transformers.EitherT
import Core
import Types
import Scope

import Data.Text (Text, pack, unpack)

-- We can use the Either transformer to handle errors
-- without switching on the either type
cdTwice :: Text -> WorldMEitherT ()
cdTwice txt = do
    _ <- liftWorldM $ changeDirectoryM txt
    _ <- liftWorldM $ changeDirectoryM txt
    return ()


main :: IO ()
main = do
    let iw = World (WorldInfo "not-set" emptyScope)
    let result = runWorldMTEither iw (cdTwice $ pack "..")
    case result of
        Left err -> print $ "Error occurred: " ++ err
        Right (_, w) -> print w