module Main where

import World
import Core
import Types
import Utility
import Scope
import Error

import Data.Text (Text, pack, unpack)
import qualified Data.Map as M

-- the lambda calculus extended with built-in functions
data Expr = Var Id
          | App Expr Expr
          | Lam Id Expr
          | BuiltIn Tag Expr
          | Lit Text -- The only type is Text
          deriving (Show)

data Tag = VPrint
         | VRead
         deriving (Show)

-- Interpreter values
data IValue = VClosure Id Expr LocalScope
           | VLit Text
           | VUnit
           | VError Text

instance Show IValue where
    show (VClosure x body env) = "VClosure " ++ x ++ " " ++ show body ++ " " ++ show env
    show (VLit t) = "VLit " ++ unpack t
    show VUnit = "VUnit"
    show (VError t) = "VError " ++ unpack t

-- Scope of the interpreter, we do not use the runtime scope here
type LocalScope = M.Map Id IValue

eval :: LocalScope -> Expr -> MiniIO IValue
-- Evaluation of literals
eval env (Lit x) = do
    return $ VLit x

-- Evaluation of variables
eval env (Var x) = do
    let val = M.lookup x env
    case M.lookup x env of
        Just v -> return v
        Nothing -> return $ VError $ pack $ "Variable " ++ x ++ " not found"

-- Implementation of built-in functions
eval env (BuiltIn x expr') =
    case x of
        VPrint -> do
            arg <- eval env expr'
            case arg of
                VLit t -> do
                    printStrM t
                    return VUnit
                _ -> fail "Expected VLit value"
        VRead -> do
            VLit <$> readStrM

-- Evaluation of lambda abstraction
eval env (Lam x body) = do
    return $ VClosure x body env

-- Evaluation of function application
eval env (App fun arg) = do
    funv <- eval env fun
    argv <- eval env arg
    case funv of
        VClosure x body clo -> do
            let env' = M.insert x argv clo
            eval env' body
        _ -> fail "Expected VClosure value"

-- Initialize the world
initWorld :: MiniIO Text
initWorld = getCwdM

main :: IO ()
main = do
    -- Initialize the world
    let iw = World (WorldInfo "not-set" emptyScope)
    let w = execIO initWorld iw

    -- Define the lambda calculus expression
    let emptyString = Lit $ pack ""
    let varX = Var "x"
    let printFunc = Lam "x" (BuiltIn VPrint varX)
    let readFunc = Lam "x" (BuiltIn VRead varX)

    -- Test expressions
    let expr1 = App printFunc (Lit $ pack "Hello World!")
    let expr2 = BuiltIn VRead emptyString

    -- Test Runtime
    let result = runIO (eval M.empty expr1) w
    print result