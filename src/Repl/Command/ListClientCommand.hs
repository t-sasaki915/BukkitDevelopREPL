module Repl.Command.ListClientCommand (ListClientCommand(ListClientCommand)) where

import           AppState
import           Repl.Command.ReplCommand (ReplCommand (..))

data ListClientCommand = ListClientCommand
                       | ListClientCommandOptions

instance ReplCommand ListClientCommand where
    cmdLabel = const "listClient"

    cmdDescription = const "Show a list of Minecraft clients that are currently running."

    cmdArgParser = const (pure (pure ListClientCommandOptions))

    cmdProcedure = listClientCommandProcedure

listClientCommandProcedure :: ListClientCommand -> AppStateIO ()
listClientCommandProcedure _ = do
    updateClientList
    clients <- getClients

    putStrLn' "These are Minecraft clients that are currently running:"
    mapM_ (\(n, _) -> putStrLn' ("  - " ++ n)) clients
