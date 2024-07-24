module Minecraft.Server.MinecraftServerSetup (setupMinecraftServer, editServerProperties) where

import           Imports

import           AppState
import           Minecraft.Server.Paper.PaperSetup   (setupPaper)
import           Minecraft.Server.Spigot.SpigotSetup (setupSpigot)

import           Data.Minecraft.MCProperty
import           Data.Minecraft.MCServerBrand        (MCServerBrand (..))

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

    lift (doesFileExist serverPropertiesFile) >>= \case
        True  -> do
            rawProperties <- lift (readFile serverPropertiesFile)

            case decodeMCProperties rawProperties of
                Right currentProperties ->
                    lift (writeFile serverPropertiesFile (encodeMCProperties (edit currentProperties)))

                Left err ->
                    error (printf "Failed to decode server.properties: %s." err)

        False -> do
            lift (writeFile serverPropertiesFile (encodeMCProperties (edit [])))

setupMinecraftServer :: AppStateIO ()
setupMinecraftServer = do
    serverBrand <- getMCServerBrand

    case serverBrand of
        Spigot -> setupSpigot
        Paper  -> setupPaper
