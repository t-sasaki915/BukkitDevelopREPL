{-# LANGUAGE OverloadedStrings #-}

module Minecraft.Client.ClientJsonAnalyser (getAssetIndex, getMainClass, getLibraries) where

import           AppState
import           CrossPlatform              (OSType (..), currentOSType)
import           FileIO
import           Minecraft.MinecraftVersion (MinecraftVersion (..))

import           Control.Monad.Trans.Except (throwE)
import           Data.Aeson
import           Data.ByteString            (fromStrict)
import           Data.Maybe                 (maybeToList)
import           System.FilePath            ((</>))

data RuleAction = Allow | Disallow deriving (Show, Eq)

instance FromJSON RuleAction where
    parseJSON (String "allow")    = pure Allow
    parseJSON (String "disallow") = pure Disallow

    parseJSON x = fail ("Failed to parse client.json: " ++ show x)

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

newtype LibraryRuleOS = LibraryRuleOS
    { libraryRuleOSName :: OSType
    }
    deriving Show

instance FromJSON LibraryRuleOS where
    parseJSON (Object m) =
        LibraryRuleOS
            <$> (m .: "name")

    parseJSON x = fail ("Failed to parse client.json: " ++ show x)

data LibraryRule = LibraryRule
    { ruleAction :: RuleAction
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
    , libraryRules     :: Maybe [LibraryRule]
    }
    deriving Show

instance FromJSON Library where
    parseJSON (Object m) =
        Library
            <$> (m .:  "downloads")
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

getLibraries :: MinecraftVersion -> AppStateIO [String]
getLibraries mcVersion = do
    clientJson <- parseClientJson mcVersion
    return (foldl gatherLibraries [] (libraries clientJson))
    where
        gatherLibraries :: [String] -> Library -> [String]
        gatherLibraries adopted = \case
            lib | shouldAdopt lib ->
                let downloads = libraryDownloads lib
                    artifact = maybeToList (downloadArtifact downloads)
                    classifier =
                        case downloadClassifiers downloads of
                            Just classifiers -> maybeToList $
                                case currentOSType of
                                    Linux   -> nativesLinuxClassifier classifiers
                                    OSX     -> nativesOSXClassifier classifiers
                                    Windows -> nativesWindowsClassifier classifiers
                            Nothing ->
                                []
                in
                adopted ++ map artifactPath (artifact ++ classifier)

            _ ->
                adopted

shouldAdopt :: Library -> Bool
shouldAdopt lib =
    case libraryRules lib of
        Just rules -> currentOSType `elem` compileLibraryRules rules
        Nothing    -> True
    where
        compileLibraryRules :: [LibraryRule] -> [OSType]
        compileLibraryRules = flip foldl [] $ \accepted rule ->
            case ruleAction rule of
                Allow ->
                    case ruleOS rule of
                        Just allowOS ->
                            let allowOSName = libraryRuleOSName allowOS in
                                if allowOSName `elem` accepted
                                    then accepted
                                    else accepted ++ [allowOSName]
                        Nothing ->
                            [Linux, OSX, Windows]

                Disallow ->
                    case ruleOS rule of
                        Just disallowOS ->
                            let disallowOSName = libraryRuleOSName disallowOS in
                                filter (/= disallowOSName) accepted
                        Nothing ->
                            []
