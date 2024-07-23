{-# LANGUAGE TemplateHaskell #-}

module Config.Resource
    ( defaultConfigFile
    , defaultApplicationWorkingDir
    , defaultApplicationAutoexec
    , defaultServerBrand
    , defaultServerVersion
    , defaultServerJvmOptions
    , defaultServerStaticPlugins
    , defaultServerPort
    , defaultServerOnlineMode
    , defaultServerMotd
    , defaultServerMaxPlayers
    , defaultServerEnableCommandBlocks
    , defaultServerDefaultGameMode
    , defaultClientDefaultVersion
    , defaultClientJvmOptions
    ) where

import           Data.ByteString              (ByteString)
import           Data.FileEmbed               (embedFile)
import           Data.Minecraft.MCGameMode    (MCGameMode (..))
import           Data.Minecraft.MCServerBrand (MCServerBrand (..))
import           Data.Minecraft.MCVersion     (MCVersion (..))

defaultConfigFile :: ByteString
defaultConfigFile = $(embedFile ".BukkitDevelopREPL.yaml")

defaultApplicationWorkingDir :: FilePath
defaultApplicationWorkingDir = "./BukkitDevelopREPL"

defaultApplicationAutoexec :: [String]
defaultApplicationAutoexec = []

defaultServerBrand :: MCServerBrand
defaultServerBrand = Paper

defaultServerVersion :: MCVersion
defaultServerVersion = MCVersion 1 20 1

defaultServerJvmOptions :: [String]
defaultServerJvmOptions =
    [ "-Xms2G"
    , "-Xmx2G"
    ]

defaultServerStaticPlugins :: [FilePath]
defaultServerStaticPlugins = []

defaultServerPort :: Int
defaultServerPort = 25565

defaultServerOnlineMode :: Bool
defaultServerOnlineMode = False

defaultServerMotd :: String
defaultServerMotd = "Plugin DEV Server"

defaultServerMaxPlayers :: Int
defaultServerMaxPlayers = 5

defaultServerEnableCommandBlocks :: Bool
defaultServerEnableCommandBlocks = True

defaultServerDefaultGameMode :: MCGameMode
defaultServerDefaultGameMode = Creative

defaultClientDefaultVersion :: MCVersion
defaultClientDefaultVersion = MCVersion 1 20 1

defaultClientJvmOptions :: [String]
defaultClientJvmOptions =
    [ "-Xms2G"
    , "-Xmx2G"
    , "-XX:+UnlockExperimentalVMOptions"
    , "-XX:+UseG1GC"
    , "-XX:G1NewSizePercent=20"
    , "-XX:G1ReservePercent=20"
    , "-XX:MaxGCPauseMillis=50"
    , "-XX:G1HeapRegionSize=32M"
    ]
