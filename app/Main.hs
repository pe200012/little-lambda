{-# LANGUAGE LambdaCase #-}
module Main where

import           Control.Monad.Except           ( runExceptT )
import           Control.Monad.State.Lazy       ( evalStateT
                                                , runStateT
                                                )
import qualified Data.Map.Lazy                 as Map
import           Data.Map.Lazy                  ( Map )
import           Eval                           ( eval
                                                , prettyprintTopLevel
                                                )
import           Lambda.Abs                     ( Term )
import           Lambda.Par                     ( myLexer
                                                , pTerm
                                                )
import           System.Console.Haskeline

-- >>> prettyprint <$> (pTerm $ myLexer "λ λ 1 (0 1 0)")
-- Right "\955 \955 1 0 1 0"

-- | repl main loop
interactive :: IO ()
interactive = do
    putStrLn "Lambda Calculus REPL 0.0.1"
    runInputT defaultSettings $ repl Map.empty
  where
    repl :: Map String Term -> InputT IO ()
    repl env = do
        -- read line
        line <- getInputLine "> "
        -- parse
        case line of
            Nothing  -> outputStrLn "Goodbye!"
            Just stm -> case pTerm $ myLexer stm of
                Left  err -> error $ show err
                Right t   -> do
                    y <- runExceptT (runStateT (eval t) env)
                    case y of
                        Left le -> do
                            outputStrLn $ "error: " ++ show le
                            repl env
                        Right (x0, env') -> runExceptT (evalStateT (prettyprintTopLevel x0) env') >>= \case
                            Left le -> do
                                outputStrLn $ "error when prettyprinting: " ++ (show le :: String)
                                repl env
                            Right x -> do
                                outputStrLn x
                                repl env'

main :: IO ()
main = interactive
