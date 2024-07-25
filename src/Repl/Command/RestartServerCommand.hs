{-# OPTIONS_GHC -Wno-partial-fields #-}

module Repl.Command.RestartServerCommand (RestartServerCommand(RestartServerCommand)) where

import           AppState
import           Repl.Command.StartServerCommand     (StartServerCommand (StartServerCommand))
import           Repl.Command.TerminateServerCommand (TerminateServerCommand (TerminateServerCommand))
import           Repl.ReplCommand                    (ReplCommand (..),
                                                      executeReplCommandInternal)

import           Options.Applicative

data RestartServerCommand = RestartServerCommand
                          | RestartServerCommandOptions
                                { withoutAsk :: Bool
                                }

instance ReplCommand RestartServerCommand where
    cmdDescription = const "Restart the Minecraft server."

    cmdArgParser = const restartServerCommandArgParser

    cmdProcedure = restartServerCommandProcedure

restartServerCommandArgParser :: AppStateIO (Parser RestartServerCommand)
restartServerCommandArgParser =
    return $
        RestartServerCommandOptions
            <$> switch
                ( long "force"
               <> short 'f'
               <> help "Restart the Minecraft server without confirming."
                )

restartServerCommandProcedure :: RestartServerCommand -> AppStateIO ()
restartServerCommandProcedure opts = do
    let force = withoutAsk opts

    executeReplCommandInternal TerminateServerCommand ["--force" | force]
    executeReplCommandInternal StartServerCommand []
