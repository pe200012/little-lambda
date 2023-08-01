{-# LANGUAGE LambdaCase #-}
module Main where

import           Control.Monad                  ( forM_ )
import           Control.Monad.Except           ( runExceptT )
import           Control.Monad.State.Lazy       ( evalStateT
                                                , runStateT
                                                )
import qualified Data.Map.Lazy                 as Map
import           Data.Map.Lazy                  ( Map
                                                , toList
                                                )
import           Eval                           ( eval'TopLevel
                                                , prettyprint
                                                , prettyprintTopLevel
                                                )
import           Lambda.Abs                     ( Term )
import           Lambda.Par                     ( myLexer
                                                , pTerm
                                                )
import           System.Console.Haskeline

-- >>> prettyprint <$> (pTerm $ myLexer "λ λ 1 (0 1 0)")
-- Right "\955 \955 1 0 1 0"

autocompletion :: Monad m => String -> m [Completion]
autocompletion "lambda" = return [simpleCompletion "λ"]
autocompletion "mu"     = return [simpleCompletion "μ"]
autocompletion _        = return []

-- | repl main loop
interactive :: IO ()
interactive = do
    putStrLn "Lambda Calculus REPL 0.0.1"
    runInputT (setComplete (completeWord Nothing " \t\n\r" autocompletion) defaultSettings { historyFile = Just "lambda.history", autoAddHistory = True })
        $ repl Map.empty
  where
    repl :: Map String Term -> InputT IO ()
    repl env = do
        -- read line
        line <- getInputLine "> "
        -- parse
        case line of
            Nothing               -> outputStrLn "Goodbye!"
            Just ":q"             -> outputStrLn "Goodbye!"
            Just ":list-bindings" -> do
                forM_ (toList env) $ \(name, term) -> do
                    outputStrLn $ name ++ " = " ++ prettyprint term
                repl env
            Just stm | all (`elem` " \n") stm  -> repl env
                     | otherwise -> case pTerm $ myLexer stm of
                Left  err -> error $ show err
                Right t   -> do
                    y <- runExceptT (runStateT (eval'TopLevel 100 t) env)
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
