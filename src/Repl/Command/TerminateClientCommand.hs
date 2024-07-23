{-# OPTIONS_GHC -Wno-partial-fields #-}

module Repl.Command.TerminateClientCommand (TerminateClientCommand(TerminateClientCommand)) where

import           Imports

import           AppState
import           Repl.ReplCommand    (ReplCommand (..), confirmContinue)

import           Data.Bifunctor      (first)
import           Options.Applicative
import           System.Process      (terminateProcess)

data TerminateClientCommand = TerminateClientCommand
                            | TerminateClientCommandOptions
                                { withoutAsk            :: Bool
                                , clientNameToTerminate :: String
                                }

instance ReplCommand TerminateClientCommand where
    cmdDescription = const "Terminate a Minecraft client."

    cmdArgParser = const terminateClientCommandArgParser

    cmdProcedure = terminateClientCommandProcedure

terminateClientCommandArgParser :: AppStateIO (Parser TerminateClientCommand)
terminateClientCommandArgParser = do
    return $
        TerminateClientCommandOptions
            <$> switch
                ( long "force"
               <> short 'f'
               <> help "Terminate a Minecraft client without confirming."
                )
            <*> argument str
                ( metavar "Name"
               <> help "Name of Minecraft client to terminate."
                )

terminateClientCommandProcedure :: TerminateClientCommand -> AppStateIO ()
terminateClientCommandProcedure opts = do
    let force             = withoutAsk opts
        clientToTerminate = clientNameToTerminate opts

    updateClientList
    clients <- getClients

    processToTerminate <- case lookup clientToTerminate (map (first runningClientName) clients) of
        Just clientProcess | force ->
            return clientProcess

        Just clientProcess -> do
            putStrLn' (printf "You are going to terminate a Minecraft client '%s'." clientToTerminate)
            putStrLn' "Unsaved changes will be discarded if you are playing Single Player mode."
            putStrLn' ""
            confirmContinue >>= \case
                True  -> return clientProcess
                False -> throwE "The operation has cancelled."

        Nothing ->
            throwE (printf "There is no Minecraft client whose name is '%s'." clientToTerminate)

    lift $ lift $ terminateProcess processToTerminate
    unregisterClient clientToTerminate

    putStrLn' (printf "Successfully terminated a Minecraft client '%s'." clientToTerminate)
