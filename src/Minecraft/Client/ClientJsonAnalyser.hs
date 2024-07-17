{-# LANGUAGE OverloadedStrings #-}

module Minecraft.Client.ClientJsonAnalyser (parseClientJson) where

import           AppState
import           FileIO
import           Minecraft.MinecraftVersion (MinecraftVersion (..))

import           Control.Monad.Trans.Except (throwE)
import           Data.Aeson
import           Data.ByteString            (fromStrict)
import           System.FilePath            ((</>))

data DownloadArtifact = DownloadArtifact
    { artifactPath :: String
    , artifactSha1 :: String
    , artifactSize :: Integer
    , artifactUrl  :: String
    }
    deriving Show

instance FromJSON DownloadArtifact where
    parseJSON (Object m) =
        DownloadArtifact
            <$> (m .: "path")
            <*> (m .: "sha1")
            <*> (m .: "size")
            <*> (m .: "url")

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

newtype LibraryExtract = LibraryExtract
    { exclude :: [String]
    }
    deriving Show

instance FromJSON LibraryExtract where
    parseJSON (Object m) =
        LibraryExtract
            <$> (m .: "exclude")

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
    , libraryName      :: String
    , libraryNatives   :: Maybe LibraryNatives
    , libraryExtract   :: Maybe LibraryExtract
    , libraryRules     :: Maybe [LibraryRule]
    }
    deriving Show

instance FromJSON Library where
    parseJSON (Object m) =
        Library
            <$> (m .:  "downloads")
            <*> (m .:  "name")
            <*> (m .:? "natives")
            <*> (m .:? "extract")
            <*> (m .:? "rules")

    parseJSON x = fail ("Failed to parse client.json: " ++ show x)

data AssetIndex = AssetIndex
    { assetIndexId        :: String
    , assetIndexSha1      :: String
    , assetIndexSize      :: Integer
    , assetIndexTotalSize :: Integer
    , assetIndexUrl       :: String
    }
    deriving Show

instance FromJSON AssetIndex where
    parseJSON (Object m) =
        AssetIndex
            <$> (m .: "id")
            <*> (m .: "sha1")
            <*> (m .: "size")
            <*> (m .: "totalSize")
            <*> (m .: "url")

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

parseClientJson :: MinecraftVersion -> AppStateIO ()
parseClientJson mcVersion = do
    versionsDir <- getMinecraftVersionsDir
    let clientJsonPath = versionsDir </> show mcVersion </> (show mcVersion ++ ".json")

    jsonContent <- readFileBS clientJsonPath $
        "Failed to read '" ++ clientJsonPath ++ "'"
    case eitherDecode (fromStrict jsonContent) :: Either String ClientJson of
        Right clientJson -> putStrLn' (show clientJson)
        Left err         -> throwE ("Failed to parse client.json: " ++ err)
