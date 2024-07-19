{-# OPTIONS_GHC -Wno-partial-fields #-}

module Repl.Command.NewClientCommand (NewClientCommand(NewClientCommand)) where

import           AppState
import           Minecraft.Client.MinecraftClient (spawnMinecraftClient)
import           Minecraft.MinecraftVersion       (MinecraftVersion (..),
                                                   minecraftVersionParser)
import           Repl.Command.ReplCommand         (ReplCommand (..))

import           Control.Monad                    (when)
import           Control.Monad.Trans.Except       (throwE)
import           Data.Bifunctor                   (first)
import           Options.Applicative

data NewClientCommand = NewClientCommand
                      | NewClientCommandOptions
                            { clientVersion  :: MinecraftVersion
                            , clientUsername :: String
                            }

instance ReplCommand NewClientCommand where
    cmdLabel = const "newClient"

    cmdDescription = const "Create a new Minecraft client."

    cmdArgParser = const newClientCommandArgParser

    cmdProcedure = newClientCommandProcedure

newClientCommandArgParser :: AppStateIO (Parser NewClientCommand)
newClientCommandArgParser = do
    defaultVersion  <- getClientDefaultVersion

    return $
        NewClientCommandOptions
            <$> option minecraftVersionParser
                ( long "version"
               <> short 'v'
               <> metavar "MinecraftVersion"
               <> value defaultVersion
               <> help ("Specifies Minecraft client version. The default is " ++ show defaultVersion)
                )
            <*> argument str
                ( metavar "Username"
               <> help "Specifies Minecraft client username."
                )

newClientCommandProcedure :: NewClientCommand -> AppStateIO ()
newClientCommandProcedure opts = do
    let version  = clientVersion opts
        username = clientUsername opts

    when (version < MinecraftVersion 1 6 1) $
        throwE "Minecraft versions older than 1.6.1 are not supported."

    updateClientList

    clients <- getClients
    case lookup username (map (first runningClientName) clients) of
        Just _ -> do
            throwE ("A Minecraft client whose name is '" ++ username ++ "' is existing.")

        Nothing -> do
            clientProcess <- spawnMinecraftClient version username
            registerNewClient (ClientInfo username version) clientProcess

            putStrLn' $
                "Successfully created a new Minecraft client with a name of '" ++ username ++ "'. The game screen will be appeared soon."
