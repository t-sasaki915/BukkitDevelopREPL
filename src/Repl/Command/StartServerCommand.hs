module Repl.Command.StartServerCommand (StartServerCommand(StartServerCommand)) where

import           AppState
import           FileIO
import           Minecraft.Server.MinecraftServer      (runMinecraftServer)
import           Minecraft.Server.MinecraftServerSetup (setupMinecraftServer)
import           Minecraft.Server.ServerBrand          (getServerExecutableName)
import           Repl.Command.ReplCommand              (ReplCommand (..))

import           System.FilePath                       ((</>))

data StartServerCommand = StartServerCommand
                        | StartServerCommandOptions

instance ReplCommand StartServerCommand where
    cmdLabel = const "startServer"

    cmdDescription = const "Start the Minecraft server."

    cmdArgParser = const (pure (pure StartServerCommandOptions))

    cmdProcedure = startServerCommandProcedure

startServerCommandProcedure :: StartServerCommand -> AppStateIO ()
startServerCommandProcedure _ = do
    workingDir    <- getWorkingDir
    serverVersion <- getServerVersion
    serverBrand   <- getServerBrand

    updateServerProc

    getServerProc >>= \case
        Just _ ->
            putStrLn' "The Minecraft server has been started already."

        Nothing -> do
            let serverJarPath = workingDir </> getServerExecutableName serverBrand serverVersion

            checkFileExistence serverJarPath
                ("Failed to check the existence of '" ++ serverJarPath ++ "'") >>= \case
                    True -> do
                        serverHandle <- runMinecraftServer
                        registerServer serverHandle

                        putStrLn' "Successfully started the Minecraft server."

                    False -> do
                        putStrLn' ("Could not find '" ++ serverJarPath ++ "'. Need to setup.")

                        setupMinecraftServer

                        putStrLn' "Successfully setup-ed the Minecraft server."
