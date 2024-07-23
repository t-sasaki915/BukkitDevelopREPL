module Repl.Repl (startRepl) where

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

import           Control.Monad                       (foldM)
import           Control.Monad.Trans.Except          (runExceptT)
import           Control.Monad.Trans.State.Strict    (runStateT)
import           Data.List.Extra                     (splitOn)
import           Data.Version                        (showVersion)
import           System.IO                           (hFlush, stdout)

import           Paths_BukkitDevelopREPL             (version)

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
        _                 -> putStrLn' ("Command '" ++ cmdName ++ "' is undefined.")
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

    (result, newState) <- runStateT (runExceptT execution) appState
    case result of
        Right () -> repLoop newState
        Left err -> putStrLn err >> repLoop newState

runAutoexec :: AppState -> IO AppState
runAutoexec appState = foldM program appState (getAutoexecCommands appState)
    where
        program :: AppState -> String -> IO AppState
        program appState' cmd = do
            putStrLn ("> " ++ cmd ++ " (autoexec)")

            let cmdName = head (splitOn " " cmd)
                cmdArgs = drop 1 (splitOn " " cmd)
                execution = execReplCommand cmdName cmdArgs

            (result, newState) <- runStateT (runExceptT execution) appState'
            case result of
                Right () -> return newState
                Left err -> putStrLn err >> return newState

startRepl :: IO ()
startRepl = do
    initState <- initialState

    putStrLn ("BukkitDevelopREPL " ++ showVersion version ++ " (" ++ show currentOSType ++ ") by TSasaki")
    putStrLn "Typing 'help' will show you the reference."
    putStrLn "Typing 'exit' is the way to quit the program gracefully."
    putStrLn ""

    newState <- runAutoexec initState

    repLoop newState
