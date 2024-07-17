module Repl.Command.ExitCommand (ExitCommand(ExitCommand)) where

import           AppState
import           Repl.Command.ReplCommand  (ReplCommand (..))

import           Control.Monad.Trans.Class (lift)
import           System.Exit               (exitSuccess)
import           System.Process            (terminateProcess)

data ExitCommand = ExitCommand
                 | ExitCommandOptions

instance ReplCommand ExitCommand where
    cmdLabel = const "exit"

    cmdDescription = const "Exit the program."

    cmdArgParser = const (pure (pure ExitCommandOptions))

    cmdProcedure = exitCommandProcedure

exitCommandProcedure :: ExitCommand -> AppStateIO ()
exitCommandProcedure _ = do
    putStrLn' "Exiting..."

    terminateAllClients

    lift $ lift exitSuccess

terminateAllClients :: AppStateIO ()
terminateAllClients = do
    updateClientList
    clients <- getClients
    mapM_ (\(_, p) -> lift $ lift $ terminateProcess p) clients
