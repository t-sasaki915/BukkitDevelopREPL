module Repl.Command.ExitCommand (ExitCommand(ExitCommand)) where

import           AppState
import           Repl.ReplCommand          (ReplCommand (..))

import           Control.Monad.Trans.Class (lift)
import           Data.Foldable             (forM_)
import           System.Exit               (exitSuccess)
import           System.Process            (terminateProcess)

data ExitCommand = ExitCommand
                 | ExitCommandOptions

instance ReplCommand ExitCommand where
    cmdDescription = const "Exit the program."

    cmdArgParser = const (pure (pure ExitCommandOptions))

    cmdProcedure = exitCommandProcedure

exitCommandProcedure :: ExitCommand -> AppStateIO ()
exitCommandProcedure _ = do
    putStrLn' "Exiting..."

    terminateAllClients
    terminateServer

    lift $ lift exitSuccess

terminateAllClients :: AppStateIO ()
terminateAllClients = do
    updateClientList
    clients <- getClients
    mapM_ (\(_, p) -> lift $ lift $ terminateProcess p) clients

terminateServer :: AppStateIO ()
terminateServer = do
    updateServerProc
    serverHandle <- getServerProc
    forM_ serverHandle (lift . lift . terminateProcess)
