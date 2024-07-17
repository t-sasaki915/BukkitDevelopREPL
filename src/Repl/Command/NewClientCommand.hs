{-# OPTIONS_GHC -Wno-partial-fields #-}

module Repl.Command.NewClientCommand (NewClientCommand(NewClientCommand)) where

import           AppState
import           Minecraft.Client.MinecraftClient (spawnMinecraftClient)
import           Minecraft.MinecraftVersion       (MinecraftVersion,
                                                   minecraftVersionParser)
import           Repl.Command.ReplCommand         (ReplCommand (..))

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
    defaultUsername <- getClientDefaultUsername

    return $
        NewClientCommandOptions
            <$> option minecraftVersionParser
                ( long "version"
               <> short 'v'
               <> metavar "MinecraftVersion"
               <> value defaultVersion
               <> help ("Specifies Minecraft client version. The default is " ++ show defaultVersion)
                )
            <*> strOption
                ( long "username"
               <> short 'u'
               <> metavar "String"
               <> value defaultUsername
               <> help ("Specifies Minecraft client username. The default is " ++ defaultUsername)
                )

newClientCommandProcedure :: NewClientCommand -> AppStateIO ()
newClientCommandProcedure opts = do
    let version  = clientVersion opts
        username = clientUsername opts

    updateClientList

    clients <- getClients
    case lookup username clients of
        Just _ -> do
            putStrLn' ("A Minecraft client whose name is '" ++ username ++ "' is existing.")
            putStrLn' "Please use '--username' option to avoid confliction."

        Nothing -> do
            clientProcess <- spawnMinecraftClient version username
            registerNewClient username clientProcess

            putStrLn' ("Successfully created a new Minecraft client with a name of '" ++ username ++ "'.")
