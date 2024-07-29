{-# LANGUAGE ScopedTypeVariables #-}

module Repl.Repl (startRepl) where

import           Imports

import           AppState
import           CLIOptions.CLIOptions               (CLIOptions (enableStacktrace))
import           CrossPlatform                       (currentOSType)
import           Repl.Command.ExitCommand            (ExitCommand (ExitCommand))
import           Repl.Command.HelpCommand            (HelpCommand (HelpCommand))
import           Repl.Command.InstallPluginsCommand  (InstallPluginsCommand (InstallPluginsCommand))
import           Repl.Command.ListClientCommand      (ListClientCommand (ListClientCommand))
import           Repl.Command.NewClientCommand       (NewClientCommand (NewClientCommand))
import           Repl.Command.OpenPluginsDirCommand  (OpenPluginsDirCommand (OpenPluginsDirCommand))
import           Repl.Command.ReloadConfigCommand    (ReloadConfigCommand (ReloadConfigCommand))
import           Repl.Command.RestartServerCommand   (RestartServerCommand (RestartServerCommand))
import           Repl.Command.ShowConfigCommand      (ShowConfigCommand (ShowConfigCommand))
import           Repl.Command.StartServerCommand     (StartServerCommand (StartServerCommand))
import           Repl.Command.TerminateClientCommand (TerminateClientCommand (TerminateClientCommand))
import           Repl.Command.TerminateServerCommand (TerminateServerCommand (TerminateServerCommand))
import           Repl.ReplCommand                    (ReplCommand (..))

import           Control.Exception                   (SomeException (..), try)
import           Control.Monad.Trans.State.Strict    (runStateT)
import           Data.List.Extra                     (dropEnd, splitOn)
import           Data.Version                        (showVersion)
import           System.Console.Haskeline
import           System.Console.Haskeline.History    (addHistory)
import           System.Exit                         (exitFailure, exitSuccess)

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
        "reloadConfig"    -> execute ReloadConfigCommand
        "newClient"       -> execute NewClientCommand
        "listClient"      -> execute ListClientCommand
        "terminateClient" -> execute TerminateClientCommand
        "startServer"     -> execute StartServerCommand
        "terminateServer" -> execute TerminateServerCommand
        "restartServer"   -> execute RestartServerCommand
        "installPlugins"  -> execute InstallPluginsCommand
        "openPluginsDir"  -> execute OpenPluginsDirCommand
        _                 -> error (printf "Command '%s' is undefined." cmdName)
        where execute c = executeReplCommand c cmdName cmdArgs

repLoop :: AppState -> InputT IO ()
repLoop appState =
    whenJustM (getInputLine "REPL> ") $ \input -> do
        let showStacktrace = enableStacktrace (_cliOptions appState)
            cmdName = head (splitOn " " input)
            cmdArgs = drop 1 (splitOn " " input)
            execution = execReplCommand cmdName cmdArgs

        result <- lift (try (runStateT execution appState))

        case result of
            Right ((), newState) ->
                repLoop newState

            Left err -> do
                handleSomeException showStacktrace err
                repLoop appState


runAutoexec :: AppState -> InputT IO AppState
runAutoexec appState = foldM program appState (getAutoexecCommands appState)
    where
        program :: AppState -> String -> InputT IO AppState
        program appState' cmd = do
            outputStrLn (printf "REPL> %s (autoexec)" cmd)
            modifyHistory (addHistory cmd)

            let showStacktrace = enableStacktrace (_cliOptions appState)
                cmdName = head (splitOn " " cmd)
                cmdArgs = drop 1 (splitOn " " cmd)
                execution = execReplCommand cmdName cmdArgs

            result <- lift (try (runStateT execution appState'))

            case result of
                Right ((), newState) ->
                    return newState

                Left err -> do
                    handleSomeException showStacktrace err
                    return appState

handleSomeException :: Bool -> SomeException -> InputT IO ()
handleSomeException showStacktrace err =
    case show err of
        s | s =~ ("^ExitSuccess$" :: String) ->
            lift exitSuccess

        s | s =~ ("^ExitFailure [0-9]+$" :: String) ->
            lift exitFailure

        errorMsg | showStacktrace ->
            outputStrLn errorMsg

        errorMsg ->
            let errorLinesWithoutStacktrace =
                    takeWhile (not . (=~ ("^CallStack .+$" :: String))) (lines errorMsg) in
                        outputStrLn (dropEnd 1 (unlines errorLinesWithoutStacktrace))

startRepl :: IO ()
startRepl = do
    initState <- initialState

    putStrLn (printf "BukkitDevelopREPL %s (%s) by TSasaki" (showVersion version) (show currentOSType))
    putStrLn "Typing 'help' will show you the reference."
    putStrLn "Typing 'exit' is the way to quit the program gracefully."
    putStrLn ""

    runInputT defaultSettings $
        runAutoexec initState >>= repLoop
