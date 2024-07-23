module Repl.Command.ListClientCommand (ListClientCommand(ListClientCommand)) where

import           Imports

import           AppState
import           Repl.ReplCommand (ReplCommand (..))

data ListClientCommand = ListClientCommand
                       | ListClientCommandOptions

instance ReplCommand ListClientCommand where
    cmdDescription = const "Show a list of Minecraft clients that are currently running."

    cmdArgParser = const (pure (pure ListClientCommandOptions))

    cmdProcedure = listClientCommandProcedure

listClientCommandProcedure :: ListClientCommand -> AppStateIO ()
listClientCommandProcedure _ = do
    updateClientList
    clients <- getClients

    putStrLn' "These are Minecraft clients that are currently running:"
    forM_ (map fst clients) $ \ci ->
        let cVersion  = runningClientVersion ci
            cName     = runningClientName ci in
            putStrLn' (printf "  - %-6s %s" (show cVersion) cName)
