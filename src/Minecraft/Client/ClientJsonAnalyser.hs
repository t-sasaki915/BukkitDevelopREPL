{-# LANGUAGE OverloadedStrings #-}

module Minecraft.Client.ClientJsonAnalyser (getAssetIndex, getMainClass) where

import           AppState
import           FileIO
import           Minecraft.MinecraftVersion (MinecraftVersion (..))

import           Control.Monad.Trans.Except (throwE)
import           Data.Aeson
import           Data.ByteString            (fromStrict)
import           System.FilePath            ((</>))

newtype DownloadArtifact = DownloadArtifact
    { artifactPath :: String
    }
    deriving Show

instance FromJSON DownloadArtifact where
    parseJSON (Object m) =
        DownloadArtifact
            <$> (m .: "path")

    parseJSON x = fail ("Failed to parse client.json: " ++ show x)

data DownloadClassifiers = DownloadClassifiers
    { nativesLinuxClassifier   :: Maybe DownloadArtifact
    , nativesOSXClassifier     :: Maybe DownloadArtifact
    , nativesWindowsClassifier :: Maybe DownloadArtifact
    }
    deriving Show

instance FromJSON DownloadClassifiers where
    parseJSON (Object m) =
        DownloadClassifiers
            <$> (m .:? "natives-linux")
            <*> (m .:? "natives-osx")
            <*> (m .:? "natives-windows")

    parseJSON x = fail ("Failed to parse client.json: " ++ show x)

data LibraryDownloads = LibraryDownloads
    { downloadArtifact    :: Maybe DownloadArtifact
    , downloadClassifiers :: Maybe DownloadClassifiers
    }
    deriving Show

instance FromJSON LibraryDownloads where
    parseJSON (Object m) =
        LibraryDownloads
            <$> (m .:? "artifact")
            <*> (m .:? "classifiers")

    parseJSON x = fail ("Failed to parse client.json" ++ show x)

data LibraryNatives = LibraryNatives
    { nativesLinux   :: Maybe String
    , nativesOSX     :: Maybe String
    , nativesWindows :: Maybe String
    }
    deriving Show

instance FromJSON LibraryNatives where
    parseJSON (Object m) =
        LibraryNatives
            <$> (m .:? "linux")
            <*> (m .:? "osx")
            <*> (m .:? "windows")

    parseJSON x = fail ("Failed to parse client.json: " ++ show x)

data LibraryRuleOS = LibraryRuleOS
    { libraryRuleOSName    :: Maybe String
    , libraryRuleOSVersion :: Maybe String
    , libraryRuleOSArch    :: Maybe String
    }
    deriving Show

instance FromJSON LibraryRuleOS where
    parseJSON (Object m) =
        LibraryRuleOS
            <$> (m .:? "name")
            <*> (m .:? "version")
            <*> (m .:? "arch")

    parseJSON x = fail ("Failed to parse client.json: " ++ show x)

data LibraryRule = LibraryRule
    { ruleAction :: String
    , ruleOS     :: Maybe LibraryRuleOS
    }
    deriving Show

instance FromJSON LibraryRule where
    parseJSON (Object m) =
        LibraryRule
            <$> (m .:  "action")
            <*> (m .:? "os")

    parseJSON x = fail ("Failed to parse client.json: " ++ show x)

data Library = Library
    { libraryDownloads :: LibraryDownloads
    , libraryNatives   :: Maybe LibraryNatives
    , libraryRules     :: Maybe [LibraryRule]
    }
    deriving Show

instance FromJSON Library where
    parseJSON (Object m) =
        Library
            <$> (m .:  "downloads")
            <*> (m .:? "natives")
            <*> (m .:? "rules")

    parseJSON x = fail ("Failed to parse client.json: " ++ show x)

newtype AssetIndex = AssetIndex
    { assetIndexId :: String
    }
    deriving Show

instance FromJSON AssetIndex where
    parseJSON (Object m) =
        AssetIndex
            <$> (m .: "id")

    parseJSON x = fail ("Failed to parse client.json: " ++ show x)

data ClientJson = ClientJson
    { assetIndex :: AssetIndex
    , libraries  :: [Library]
    , mainClass  :: String
    }
    deriving Show

instance FromJSON ClientJson where
    parseJSON (Object m) =
        ClientJson
            <$> (m .: "assetIndex")
            <*> (m .: "libraries")
            <*> (m .: "mainClass")

    parseJSON x = fail ("Failed to parse client.json: " ++ show x)

parseClientJson :: MinecraftVersion -> AppStateIO ClientJson
parseClientJson mcVersion = do
    versionsDir <- getMinecraftVersionsDir
    let clientJsonPath = versionsDir </> show mcVersion </> (show mcVersion ++ ".json")

    jsonContent <- readFileBS clientJsonPath $
        "Failed to read '" ++ clientJsonPath ++ "'"
    case eitherDecode (fromStrict jsonContent) :: Either String ClientJson of
        Right clientJson -> return clientJson
        Left err         -> throwE ("Failed to parse client.json: " ++ err)

getAssetIndex :: MinecraftVersion -> AppStateIO String
getAssetIndex mcVersion = do
    clientJson <- parseClientJson mcVersion
    return (assetIndexId (assetIndex clientJson))

getMainClass :: MinecraftVersion -> AppStateIO String
getMainClass mcVersion = do
    clientJson <- parseClientJson mcVersion
    return (mainClass clientJson)
