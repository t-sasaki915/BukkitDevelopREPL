{-# OPTIONS_GHC -Wno-partial-fields #-}

module Repl.Command.TerminateServerCommand (TerminateServerCommand(TerminateServerCommand)) where

import           AppState
import           Repl.ReplCommand           (ReplCommand (..), confirmContinue)

import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Except (throwE)
import           Options.Applicative
import           System.Process             (terminateProcess)

data TerminateServerCommand = TerminateServerCommand
                            | TerminateServerCommandOptions
                                { withoutAsk :: Bool
                                }

instance ReplCommand TerminateServerCommand where
    cmdDescription = const "Terminate the Minecraft server."

    cmdArgParser = const terminateServerCommandArgParser

    cmdProcedure = terminateServerCommandProcedure

terminateServerCommandArgParser :: AppStateIO (Parser TerminateServerCommand)
terminateServerCommandArgParser =
    return $
        TerminateServerCommandOptions
            <$> switch
                ( long "force"
               <> short 'f'
               <> help "Terminate the Minecraft server without confirming."
                )

terminateServerCommandProcedure :: TerminateServerCommand -> AppStateIO ()
terminateServerCommandProcedure opts = do
    let force = withoutAsk opts

    updateServerProc

    processToTerminate <- getServerProc >>= \case
        Just serverHandle | force ->
            return serverHandle

        Just serverHandle -> do
            putStrLn' "You are going to terminate the Minecraft server."
            putStrLn' "Unsaved changes will be discarded."
            putStrLn' ""
            confirmContinue >>= \case
                True  -> return serverHandle
                False -> throwE "The operation has cancelled."

        Nothing ->
            throwE "The Minecraft server is not running."

    lift $ lift $ terminateProcess processToTerminate
    unregisterServer

    putStrLn' "Successfully terminated the Minecraft server."
