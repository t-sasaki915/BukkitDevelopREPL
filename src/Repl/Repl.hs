module Repl.Repl (startRepl) where

import           AppState
import           Repl.Command.ReplCommand

import           Control.Monad.Trans.Class      (lift)
import           Data.List.Split                (splitOn)
import           Data.Version                   (showVersion)
import           System.IO                      (hFlush, stdout)

import           Paths_spigot_debugger_launcher (version)

execReplCommand :: String -> [String] -> AppStateIO ()
execReplCommand cmdName cmdArgs =
    case lookup cmdName (map (\c -> (cmdLabel c, c)) replCommands) of
        Just cmd -> do
            cmdProcedure cmd cmdArgs >>= \case
                True  -> return ()
                False -> do
                    putStrLn' ("Invalid usage of '" ++ cmdName ++ "'.")
                    putStrLn' ("Usage: " ++ cmdName ++ " " ++ cmdUsage cmd)

        Nothing  ->
            putStrLn' ("Undefined command: " ++ cmdName)

repLoop :: AppStateIO ()
repLoop = do
    input <- lift $ lift $ do
        putStr "> "
        hFlush stdout
        getLine

    let cmdName = head (splitOn " " input)
        cmdArgs = drop 1 (splitOn " " input)
    execReplCommand cmdName cmdArgs

    repLoop

startRepl :: AppStateIO ()
startRepl = do
    putStrLn' ("spigot-debugger-launcher REPL " ++ showVersion version ++ " by TSasaki")
    putStrLn' "Typing 'help' will show you the reference."
    putStrLn' "Typing 'exit' is the way to quit the program gracefully."
    putStrLn' ""
    repLoop
