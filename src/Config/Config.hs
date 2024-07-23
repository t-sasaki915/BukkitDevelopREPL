module Config.Config
    ( Config(..)
    , ApplicationConfig(..)
    , ServerConfig(..)
    , ClientConfig(..)
    ) where

import           Config.Resource

import           Data.Minecraft.MCGameMode    (MCGameMode)
import           Data.Minecraft.MCServerBrand (MCServerBrand)
import           Data.Minecraft.MCVersion     (MCVersion)
import           Data.Yaml

data Config = Config
    { applicationConfig :: ApplicationConfig
    , serverConfig      :: ServerConfig
    , clientConfig      :: ClientConfig
    }
    deriving Show

instance FromJSON Config where
    parseJSON (Object m) =
        Config
            <$> m .:? "application" .!= defaultApplicationConfig
            <*> m .:? "server"      .!= defaultServerConfig
            <*> m .:? "client"      .!= defaultClientConfig

    parseJSON Null = return defaultConfig

    parseJSON _ = fail "Unrecognisable config"

instance ToJSON Config where
    toJSON conf =
        object
            [ "application" .= applicationConfig conf
            , "server"      .= serverConfig conf
            , "client"      .= clientConfig conf
            ]

defaultConfig :: Config
defaultConfig =
    Config
        defaultApplicationConfig
        defaultServerConfig
        defaultClientConfig

data ApplicationConfig = ApplicationConfig
    { workingDir       :: FilePath
    , autoexecCommands :: [String]
    }
    deriving Show

instance FromJSON ApplicationConfig where
    parseJSON (Object m) =
        ApplicationConfig
            <$> m .:? "workingDir" .!= defaultApplicationWorkingDir
            <*> m .:? "autoexec"   .!= defaultApplicationAutoexec

    parseJSON _ = fail "Unrecognisable application config"

instance ToJSON ApplicationConfig where
    toJSON conf =
        object
            [ "workingDir" .= workingDir conf
            , "autoexec"   .= autoexecCommands conf
            ]

defaultApplicationConfig :: ApplicationConfig
defaultApplicationConfig =
    ApplicationConfig
        defaultApplicationWorkingDir
        defaultApplicationAutoexec

data ServerConfig = ServerConfig
    { serverBrand               :: MCServerBrand
    , serverVersion             :: MCVersion
    , serverJvmOptions          :: [String]
    , serverStaticPlugins       :: [FilePath]
    , serverPort                :: Int
    , serverOnlineMode          :: Bool
    , serverMotd                :: String
    , serverMaxPlayers          :: Int
    , serverEnableCommandBlocks :: Bool
    , serverDefaultGameMode     :: MCGameMode
    }
    deriving Show

instance FromJSON ServerConfig where
    parseJSON (Object m) =
        ServerConfig
            <$> m .:? "brand"               .!= defaultServerBrand
            <*> m .:? "version"             .!= defaultServerVersion
            <*> m .:? "jvmOptions"          .!= defaultServerJvmOptions
            <*> m .:? "staticPlugins"       .!= defaultServerStaticPlugins
            <*> m .:? "port"                .!= defaultServerPort
            <*> m .:? "onlineMode"          .!= defaultServerOnlineMode
            <*> m .:? "motd"                .!= defaultServerMotd
            <*> m .:? "maxPlayers"          .!= defaultServerMaxPlayers
            <*> m .:? "enableCommandBlocks" .!= defaultServerEnableCommandBlocks
            <*> m .:? "defaultGameMode"     .!= defaultServerDefaultGameMode

    parseJSON _ = fail "Unrecognisable server config"

instance ToJSON ServerConfig where
    toJSON conf =
        object
            [ "brand"         .= serverBrand conf
            , "version"       .= serverVersion conf
            , "jvmOptions"    .= serverJvmOptions conf
            , "staticPlugins" .= serverStaticPlugins conf
            ]

defaultServerConfig :: ServerConfig
defaultServerConfig =
    ServerConfig
        defaultServerBrand
        defaultServerVersion
        defaultServerJvmOptions
        defaultServerStaticPlugins
        defaultServerPort
        defaultServerOnlineMode
        defaultServerMotd
        defaultServerMaxPlayers
        defaultServerEnableCommandBlocks
        defaultServerDefaultGameMode

data ClientConfig = ClientConfig
    { clientDefaultVersion :: MCVersion
    , clientJvmOptions     :: [String]
    }
    deriving Show

instance FromJSON ClientConfig where
    parseJSON (Object m) =
        ClientConfig
            <$> m .:? "defaultVersion" .!= defaultClientDefaultVersion
            <*> m .:? "jvmOptions"     .!= defaultClientJvmOptions

    parseJSON _ = fail "Unrecognisable client config"

instance ToJSON ClientConfig where
    toJSON conf =
        object
            [ "defaultVersion" .= clientDefaultVersion conf
            , "jvmOptions"     .= clientJvmOptions conf
            ]

defaultClientConfig :: ClientConfig
defaultClientConfig =
    ClientConfig
        defaultClientDefaultVersion
        defaultClientJvmOptions
