module Repl.Command.ReloadConfigCommand (ReloadConfigCommand(ReloadConfigCommand)) where

import           Imports

import           AppState
import           Config.Loader    (loadConfig)
import           Repl.ReplCommand (ReplCommand (..))

data ReloadConfigCommand = ReloadConfigCommand
                         | ReloadConfigCommandOptions

instance ReplCommand ReloadConfigCommand where
    cmdDescription = const "Reload the configurations."

    cmdArgParser = const (pure (pure ReloadConfigCommandOptions))

    cmdProcedure = reloadConfigCommandProcedure

reloadConfigCommandProcedure :: ReloadConfigCommand -> AppStateIO ()
reloadConfigCommandProcedure _ = do
    configFilePath <- getConfigFilePath
    newConfig      <- lift (loadConfig configFilePath)

    setConfig newConfig

    putStrLn' "Successfully reloaded the configurations."
