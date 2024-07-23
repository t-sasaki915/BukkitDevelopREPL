module Config.Config
    ( Config(..)
    , ApplicationConfig(..)
    , ServerConfig(..)
    , ClientConfig(..)
    ) where

import           Config.Resource

import           Data.Minecraft.MCServerBrand (MCServerBrand)
import           Data.Minecraft.MCVersion     (MCVersion (..))
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
    toJSON (Config cApplicationConfig cServerConfig cClientConfig) =
        object
            [ "application" .= cApplicationConfig
            , "server"      .= cServerConfig
            , "client"      .= cClientConfig
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
    toJSON (ApplicationConfig aWorkingDir aAutoexecCommands) =
        object
            [ "workingDir" .= aWorkingDir
            , "autoexec"   .= aAutoexecCommands
            ]

defaultApplicationConfig :: ApplicationConfig
defaultApplicationConfig =
    ApplicationConfig
        defaultApplicationWorkingDir
        defaultApplicationAutoexec

data ServerConfig = ServerConfig
    { serverBrand         :: MCServerBrand
    , serverVersion       :: MCVersion
    , serverJvmOptions    :: [String]
    , serverStaticPlugins :: [FilePath]
    }
    deriving Show

instance FromJSON ServerConfig where
    parseJSON (Object m) =
        ServerConfig
            <$> m .:? "brand"         .!= defaultServerBrand
            <*> m .:? "version"       .!= defaultServerVersion
            <*> m .:? "jvmOptions"    .!= defaultServerJvmOptions
            <*> m .:? "staticPlugins" .!= defaultServerStaticPlugins

    parseJSON _ = fail "Unrecognisable server config"

instance ToJSON ServerConfig where
    toJSON (ServerConfig sBrand sVersion sJvmOptions sStaticPlugins) =
        object
            [ "brand"         .= sBrand
            , "version"       .= sVersion
            , "jvmOptions"    .= sJvmOptions
            , "staticPlugins" .= sStaticPlugins
            ]

defaultServerConfig :: ServerConfig
defaultServerConfig =
    ServerConfig
        defaultServerBrand
        defaultServerVersion
        defaultServerJvmOptions
        defaultServerStaticPlugins

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
    toJSON (ClientConfig cDefaultVersion cJvmOptions) =
        object
            [ "defaultVersion" .= cDefaultVersion
            , "jvmOptions"     .= cJvmOptions
            ]

defaultClientConfig :: ClientConfig
defaultClientConfig =
    ClientConfig
        defaultClientDefaultVersion
        defaultClientJvmOptions
