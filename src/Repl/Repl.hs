module Repl.Repl (startRepl) where

import           AppState
import           Repl.Command.ExitCommand       (ExitCommand (ExitCommand))
import           Repl.Command.HelpCommand       (HelpCommand (HelpCommand))
import           Repl.Command.ReplCommand       (ReplCommand (..))

import           Control.Monad.Trans.Class      (lift)
import           Data.List.Split                (splitOn)
import           Data.Version                   (showVersion)
import           Options.Applicative
import           System.IO                      (hFlush, stdout)

import           Paths_spigot_debugger_launcher (version)

execReplCommand :: String -> [String] -> AppStateIO ()
execReplCommand cmdName cmdArgs =
    case cmdName of
        "help" -> execute HelpCommand
        "exit" -> execute ExitCommand
        _      -> putStrLn' ("Command '" ++ cmdName ++ "' is undefined.")
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
