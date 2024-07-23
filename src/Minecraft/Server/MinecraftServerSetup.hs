module Minecraft.Server.MinecraftServerSetup (setupMinecraftServer) where

import           AppState
import           FileIO
import           Minecraft.Server.Paper.PaperSetup   (setupPaper)
import           Minecraft.Server.ServerBrand        (ServerBrand (..))
import           Minecraft.Server.Spigot.SpigotSetup (setupSpigot)

import           Data.MCProperty
import           System.FilePath                     ((</>))

generateInitialServerProperties :: AppStateIO ()
generateInitialServerProperties = do
    workingDir <- getWorkingDir

    let serverPropertiesFile = workingDir </> "server.properties"

    checkFileExistence serverPropertiesFile
        ("Failed to check the existence of '" ++ serverPropertiesFile ++ "'") >>= \case
            True  -> return ()
            False -> do
                let
                    initProperties = newMCProperties $ do
                        addProperty "gamemode"             (MCString "creative")
                        addProperty "motd"                 (MCString "Plugin DEV Server")
                        addProperty "enable-command-block" (MCBool True)
                        addProperty "online-mode"          (MCBool False)

                writeFile' serverPropertiesFile (encodeMCProperties initProperties)
                    "Failed to generate server.properties"

setupMinecraftServer :: AppStateIO ()
setupMinecraftServer = do
    serverBrand <- getServerBrand

    case serverBrand of
        Spigot -> setupSpigot
        Paper  -> setupPaper

    generateInitialServerProperties
