module Repl.Repl (startRepl) where

import           AppState
import           Repl.Command.ExitCommand            (ExitCommand (ExitCommand))
import           Repl.Command.HelpCommand            (HelpCommand (HelpCommand))
import           Repl.Command.ListClientCommand      (ListClientCommand (ListClientCommand))
import           Repl.Command.NewClientCommand       (NewClientCommand (NewClientCommand))
import           Repl.Command.ReplCommand            (ReplCommand (..))
import           Repl.Command.TerminateClientCommand (TerminateClientCommand (TerminateClientCommand))

import           Control.Monad.Trans.Except          (runExceptT)
import           Control.Monad.Trans.State.Strict    (runStateT)
import           Data.List.Split                     (splitOn)
import           Data.Version                        (showVersion)
import           Options.Applicative
import           System.IO                           (hFlush, stdout)

import           Paths_spigot_debugger_launcher      (version)

execReplCommand :: String -> [String] -> AppStateIO ()
execReplCommand cmdName cmdArgs =
    case cmdName of
        "help"            -> execute HelpCommand
        "exit"            -> execute ExitCommand
        "newClient"       -> execute NewClientCommand
        "listClient"      -> execute ListClientCommand
        "terminateClient" -> execute TerminateClientCommand
        _                 -> putStrLn' ("Command '" ++ cmdName ++ "' is undefined.")
    where
        execute :: ReplCommand c => c -> AppStateIO ()
        execute cmd = do
            parser <- cmdArgParser cmd
            let executor =
                    execParserPure
                        (prefs disambiguate)
                            (info (helper <*> parser)
                                (fullDesc <> progDesc (cmdDescription cmd)))
            case executor cmdArgs of
                Success parsedArgs ->
                    cmdProcedure parsedArgs

                Failure err ->
                    let (helpMsg, _, _) = execFailure err (cmdLabel cmd) in
                        putStrLn' (show helpMsg)

                _ ->
                    return ()

repLoop :: AppState -> IO ()
repLoop appState = do
    input <- do
        putStr "> "
        hFlush stdout
        getLine

    let cmdName = head (splitOn " " input)
        cmdArgs = drop 1 (splitOn " " input)
        execution = execReplCommand cmdName cmdArgs

    (result, newState) <- runStateT (runExceptT execution) appState
    case result of
        Right () -> repLoop newState
        Left err -> putStrLn err >> repLoop newState

startRepl :: IO ()
startRepl = do
    initState <- initialState

    putStrLn ("spigot-debugger-launcher REPL " ++ showVersion version ++ " by TSasaki")
    putStrLn "Typing 'help' will show you the reference."
    putStrLn "Typing 'exit' is the way to quit the program gracefully."
    putStrLn ""

    repLoop initState
