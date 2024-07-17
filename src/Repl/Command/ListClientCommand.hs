module Repl.Command.ListClientCommand (ListClientCommand(ListClientCommand)) where

import           AppState
import           Repl.Command.ReplCommand (ReplCommand (..))
import           Util                     (fillWithSpace)

import           Control.Monad            (forM_)

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
    forM_ (map fst clients) $ \ci ->
        let cVersion = runningClientVersion ci
            cName    = runningClientName ci in
            putStrLn' ("  - " ++ fillWithSpace 6 (show cVersion) ++ " " ++ cName)
