module Minecraft.Server.MinecraftServerSetup (setupMinecraftServer, editServerProperties) where

import           AppState
import           FileIO
import           Minecraft.Server.Paper.PaperSetup   (setupPaper)
import           Minecraft.Server.Spigot.SpigotSetup (setupSpigot)

import           Control.Monad.Trans.Except          (throwE)
import           Data.Minecraft.MCProperty
import           Data.Minecraft.MCServerBrand        (MCServerBrand (..))
import           System.FilePath                     ((</>))

editServerProperties :: AppStateIO ()
editServerProperties = do
    workingDir          <- getWorkingDir
    port                <- getServerPort
    onlineMode          <- shouldServerUseOnlineMode
    motd                <- getServerMotd
    maxPlayers          <- getServerMaxPlayers
    enableCommandBlocks <- shouldServerEnableCommandBlocks
    defaultGameMode     <- getServerDefaultGameMode

    let
        edit = editMCProperties $ do
            setProperty "server-port"          (MCInt port)
            setProperty "online-mode"          (MCBool onlineMode)
            setProperty "motd"                 (MCString motd)
            setProperty "max-players"          (MCInt maxPlayers)
            setProperty "enable-command-block" (MCBool enableCommandBlocks)
            setProperty "gamemode"             (MCGameMode defaultGameMode)

        serverPropertiesFile = workingDir </> "server.properties"

    checkFileExistence serverPropertiesFile
        ("Failed to check the existence of '" ++ serverPropertiesFile ++ "'") >>= \case
            True  -> do
                rawProperties <- readFile' serverPropertiesFile $
                    "Failed to read a file '" ++ serverPropertiesFile ++ "'"

                case decodeMCProperties rawProperties of
                    Right currentProperties ->
                        writeFile' serverPropertiesFile (encodeMCProperties (edit currentProperties))
                            "Failed to edit server.properties"

                    Left err ->
                        throwE ("Failed to decode server.properties: " ++ err)

            False -> do
                writeFile' serverPropertiesFile (encodeMCProperties (edit []))
                    "Failed to generate server.properties"

setupMinecraftServer :: AppStateIO ()
setupMinecraftServer = do
    serverBrand <- getMCServerBrand

    case serverBrand of
        Spigot -> setupSpigot
        Paper  -> setupPaper
