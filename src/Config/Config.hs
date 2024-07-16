{-# LANGUAGE OverloadedStrings #-}

module Config.Config (Config(..), ServerConfig(..), ClientConfig(..)) where

import           Minecraft.MinecraftVersion (MinecraftVersion)

import           Data.Yaml                  (FromJSON (..), Value (..), (.:))

data Config = Config
    { serverConfig :: ServerConfig
    , clientConfig :: ClientConfig
    }
    deriving Show

data ServerConfig = ServerConfig
    { serverVersion       :: MinecraftVersion
    , serverJvmOptions    :: [String]
    , serverOnlineMode    :: Bool
    , serverStaticPlugins :: [FilePath]
    }
    deriving Show

data ClientConfig = ClientConfig
    { clientDefaultVersion :: MinecraftVersion
    , clientJvmOptions     :: [String]
    }
    deriving Show

instance FromJSON ClientConfig where
    parseJSON (Object m) =
        ClientConfig
            <$> m .: "defaultVersion"
            <*> m .: "jvmOptions"

    parseJSON _ = fail "Unrecognisable client config"

instance FromJSON ServerConfig where
    parseJSON (Object m) =
        ServerConfig
            <$> m .: "version"
            <*> m .: "jvmOptions"
            <*> m .: "onlineMode"
            <*> m .: "staticPlugins"

    parseJSON _ = fail "Unrecognisable server config"

instance FromJSON Config where
    parseJSON (Object m) =
        Config
            <$> m .: "server"
            <*> m .: "client"

    parseJSON _ = fail "Unrecognisable config"
