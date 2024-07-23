module Config.Config
    ( Config(..)
    , ApplicationConfig(..)
    , ServerConfig(..)
    , ClientConfig(..)
    ) where

import           Config.Resource

import           Data.Minecraft.MCServerBrand (MCServerBrand)
import           Data.Minecraft.MCVersion     (MCVersion (..))
import           Data.Yaml                    (FromJSON (..), Value (..), (.!=),
                                               (.:))

data Config = Config
    { applicationConfig :: ApplicationConfig
    , serverConfig      :: ServerConfig
    , clientConfig      :: ClientConfig
    }
    deriving Show

instance FromJSON Config where
    parseJSON (Object m) =
        Config
            <$> m .: "application" .!= defaultApplicationConfig
            <*> m .: "server"      .!= defaultServerConfig
            <*> m .: "client"      .!= defaultClientConfig

    parseJSON _ = fail "Unrecognisable config"

data ApplicationConfig = ApplicationConfig
    { workingDir       :: FilePath
    , autoexecCommands :: [String]
    }
    deriving Show

instance FromJSON ApplicationConfig where
    parseJSON (Object m) =
        ApplicationConfig
            <$> m .: "workingDir" .!= defaultApplicationWorkingDir
            <*> m .: "autoexec"   .!= defaultApplicationAutoexec

    parseJSON _ = fail "Unrecognisable application config"

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
            <$> m .: "brand"         .!= defaultServerBrand
            <*> m .: "version"       .!= defaultServerVersion
            <*> m .: "jvmOptions"    .!= defaultServerJvmOptions
            <*> m .: "staticPlugins" .!= defaultServerStaticPlugins

    parseJSON _ = fail "Unrecognisable server config"

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
            <$> m .: "defaultVersion" .!= defaultClientDefaultVersion
            <*> m .: "jvmOptions"     .!= defaultClientJvmOptions

    parseJSON _ = fail "Unrecognisable client config"

defaultClientConfig :: ClientConfig
defaultClientConfig =
    ClientConfig
        defaultClientDefaultVersion
        defaultClientJvmOptions
