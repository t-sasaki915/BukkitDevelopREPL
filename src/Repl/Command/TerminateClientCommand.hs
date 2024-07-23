{-# OPTIONS_GHC -Wno-partial-fields #-}

module Repl.Command.TerminateClientCommand (TerminateClientCommand(TerminateClientCommand)) where

import           AppState
import           Repl.ReplCommand           (ReplCommand (..), confirmContinue)

import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Except (throwE)
import           Data.Bifunctor             (first)
import           Options.Applicative
import           System.Process             (terminateProcess)

data TerminateClientCommand = TerminateClientCommand
                            | TerminateClientCommandOptions
                                { withoutAsk            :: Bool
                                , clientNameToTerminate :: String
                                }

instance ReplCommand TerminateClientCommand where
    cmdLabel = const "terminateClient"

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

    case lookup clientToTerminate (map (first runningClientName) clients) of
        Just clientProcess | force -> do
            lift $ lift $ terminateProcess clientProcess
            putStrLn' ("Successfully terminated a Minecraft client '" ++ clientToTerminate ++ "'.")

        Just clientProcess -> do
            putStrLn' ("You are going to terminate a Minecraft client '" ++ clientToTerminate ++ "'.")
            putStrLn' "Unsaved changes will be discarded if you are playing Single Player mode."
            putStrLn' ""
            confirmContinue >>= \case
                True -> do
                    lift $ lift $ terminateProcess clientProcess
                    putStrLn' ("Successfully terminated a Minecraft client '" ++ clientToTerminate ++ "'.")

                False -> do
                    throwE "The operation has cancelled."

        Nothing ->
            throwE ("There is no Minecraft client whose name is '" ++ clientToTerminate ++ "'.")
