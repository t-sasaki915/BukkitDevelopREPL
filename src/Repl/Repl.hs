{-# LANGUAGE ScopedTypeVariables #-}

module Repl.Repl (startRepl) where

import           Imports

import           AppState
import           CrossPlatform                       (currentOSType)
import           Repl.Command.ExitCommand            (ExitCommand (ExitCommand))
import           Repl.Command.HelpCommand            (HelpCommand (HelpCommand))
import           Repl.Command.InstallPluginsCommand  (InstallPluginsCommand (InstallPluginsCommand))
import           Repl.Command.ListClientCommand      (ListClientCommand (ListClientCommand))
import           Repl.Command.NewClientCommand       (NewClientCommand (NewClientCommand))
import           Repl.Command.ShowConfigCommand      (ShowConfigCommand (ShowConfigCommand))
import           Repl.Command.StartServerCommand     (StartServerCommand (StartServerCommand))
import           Repl.Command.TerminateClientCommand (TerminateClientCommand (TerminateClientCommand))
import           Repl.Command.TerminateServerCommand (TerminateServerCommand (TerminateServerCommand))
import           Repl.ReplCommand                    (ReplCommand (..))

import           Control.Exception                   (SomeException (..), try)
import           Control.Monad.Trans.State.Strict    (runStateT)
import           Data.List.Extra                     (splitOn)
import           Data.Version                        (showVersion)
import           System.Exit                         (exitFailure, exitSuccess)
import           System.IO                           (hFlush, stdout)

execReplCommand :: String -> [String] -> AppStateIO ()
execReplCommand cmdName cmdArgs =
    case cmdName of
        "help"            -> execute HelpCommand
        "?"               -> execute HelpCommand
        "listCommand"     -> execute HelpCommand
        "exit"            -> execute ExitCommand
        "quit"            -> execute ExitCommand
        "stop"            -> execute ExitCommand
        "showConfig"      -> execute ShowConfigCommand
        "newClient"       -> execute NewClientCommand
        "listClient"      -> execute ListClientCommand
        "terminateClient" -> execute TerminateClientCommand
        "startServer"     -> execute StartServerCommand
        "terminateServer" -> execute TerminateServerCommand
        "installPlugins"  -> execute InstallPluginsCommand
        _                 -> error (printf "Command '%s' is undefined." cmdName)
        where execute c = executeReplCommand c cmdName cmdArgs

repLoop :: AppState -> IO ()
repLoop appState = do
    input <- do
        putStr "> "
        hFlush stdout
        getLine

    let cmdName = head (splitOn " " input)
        cmdArgs = drop 1 (splitOn " " input)
        execution = execReplCommand cmdName cmdArgs

    result <- try (runStateT execution appState) :: IO (Either SomeException ((), AppState))

    case result of
        Right ((), newState) ->
            repLoop newState

        Left err ->
            case show err of
                s | s =~ ("^ExitSuccess$" :: String) ->
                    exitSuccess

                s | s =~ ("^ExitFailure [0-9]+$" :: String) ->
                    exitFailure

                errorMsg -> do
                    putStrLn errorMsg
                    repLoop appState


runAutoexec :: AppState -> IO AppState
runAutoexec appState = foldM program appState (getAutoexecCommands appState)
    where
        program :: AppState -> String -> IO AppState
        program appState' cmd = do
            putStrLn (printf "> %s (autoexec)" cmd)

            let cmdName = head (splitOn " " cmd)
                cmdArgs = drop 1 (splitOn " " cmd)
                execution = execReplCommand cmdName cmdArgs

            result <- try (runStateT execution appState') :: IO (Either SomeException ((), AppState))

            case result of
                Right ((), newState) ->
                    return newState

                Left err ->
                    case show err of
                        s | s =~ ("^ExitSuccess$" :: String) ->
                            exitSuccess

                        s | s =~ ("^ExitFailure [0-9]+$" :: String) ->
                            exitFailure

                        errorMsg -> do
                            putStrLn errorMsg
                            return appState

startRepl :: IO ()
startRepl = do
    initState <- initialState

    putStrLn (printf "BukkitDevelopREPL %s (%s) by TSasaki" (showVersion version) (show currentOSType))
    putStrLn "Typing 'help' will show you the reference."
    putStrLn "Typing 'exit' is the way to quit the program gracefully."
    putStrLn ""

    runAutoexec initState >>= repLoop
