module Minecraft.Server.MinecraftServerSetup (setupMinecraftServer) where

import           AppState
import           FileIO
import           Minecraft.MinecraftPropertyAnalyser
import           Minecraft.Server.Paper.PaperSetup   (setupPaper)
import           Minecraft.Server.ServerBrand        (ServerBrand (..))
import           Minecraft.Server.Spigot.SpigotSetup (setupSpigot)

import           System.FilePath                     ((</>))

generateInitialServerProperties :: AppStateIO ()
generateInitialServerProperties = do
    workingDir <- getWorkingDir

    let serverPropertiesFile = workingDir </> "server.properties"

    checkFileExistence serverPropertiesFile
        ("Failed to check the existence of '" ++ serverPropertiesFile ++ "'") >>= \case
            True  -> return ()
            False -> do
                let initProperties =
                        [ MCProperty "gamemode"             (MCString "creative")
                        , MCProperty "motd"                 (MCString "Plugin DEV Server")
                        , MCProperty "enable-command-block" (MCBool True)
                        , MCProperty "online-mode"          (MCBool False)
                        ]
                    encoded = encodeMinecraftProperty initProperties

                writeFile' serverPropertiesFile encoded
                    "Failed to generate server.properties"

setupMinecraftServer :: AppStateIO ()
setupMinecraftServer = do
    serverBrand <- getServerBrand

    case serverBrand of
        Spigot -> setupSpigot
        Paper  -> setupPaper

    generateInitialServerProperties
