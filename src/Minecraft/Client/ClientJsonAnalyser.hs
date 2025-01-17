module Minecraft.Client.ClientJsonAnalyser (getAssetIndex, getMainClass, getLibraries) where

import           Imports

import           AppState
import           CrossPlatform            (OSType (..), currentOSType)

import           Data.Aeson
import qualified Data.ByteString          as BS
import           Data.Maybe               (maybeToList)
import           Data.Minecraft.MCVersion (MCVersion (..))

data RuleAction = Allow | Disallow deriving (Show, Eq)

instance FromJSON RuleAction where
    parseJSON (String "allow")    = pure Allow
    parseJSON (String "disallow") = pure Disallow
    parseJSON x = fail (printf "Unrecognisable rule action '%s'." (show x))

newtype DownloadArtifact = DownloadArtifact
    { artifactPath :: String
    }
    deriving Show

instance FromJSON DownloadArtifact where
    parseJSON (Object m) =
        DownloadArtifact
            <$> (m .: "path")

    parseJSON x = fail (printf "Unrecognisable download artifact '%s'." (show x))

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

    parseJSON x = fail (printf "Unrecognisable download classifiers '%s'." (show x))

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

    parseJSON x = fail (printf "Unrecognisable library downloads '%s'." (show x))

newtype LibraryRuleOS = LibraryRuleOS
    { libraryRuleOSName :: OSType
    }
    deriving Show

instance FromJSON LibraryRuleOS where
    parseJSON (Object m) =
        LibraryRuleOS
            <$> (m .: "name")

    parseJSON x = fail (printf "Unrecognisable library rule os '%s'." (show x))

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

    parseJSON x = fail (printf "Unrecognisable library rule '%s'." (show x))

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

    parseJSON x = fail (printf "Unrecognisable library '%s'." (show x))

newtype AssetIndex = AssetIndex
    { assetIndexId :: String
    }
    deriving Show

instance FromJSON AssetIndex where
    parseJSON (Object m) =
        AssetIndex
            <$> (m .: "id")

    parseJSON x = fail (printf "Unrecognisable asset index '%s'." (show x))

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

    parseJSON x = fail (printf "Unrecognisable client.json '%s'." (show x))

getMinecraftVersionsDir :: AppStateIO FilePath
getMinecraftVersionsDir = getMinecraftDir <&> (</> "versions")

parseClientJson :: MCVersion -> AppStateIO ClientJson
parseClientJson mcVersion = do
    versionsDir <- getMinecraftVersionsDir
    let clientJsonPath = versionsDir </> show mcVersion </> printf "%s.json" (show mcVersion)

    unlessM (lift (doesFileExist clientJsonPath)) $
        error $ printf
            "Could not find '%s'. You need to launch Minecraft %s at least once with the official Minecraft Launcher."
                clientJsonPath (show mcVersion)

    jsonContent <- lift (BS.readFile clientJsonPath)
    case eitherDecode (BS.fromStrict jsonContent) :: Either String ClientJson of
        Right clientJson -> return clientJson
        Left err         -> error (printf "Failed to decode client.json: %s" err)

getAssetIndex :: MCVersion -> AppStateIO String
getAssetIndex mcVersion = do
    clientJson <- parseClientJson mcVersion
    return (assetIndexId (assetIndex clientJson))

getMainClass :: MCVersion -> AppStateIO String
getMainClass mcVersion = do
    clientJson <- parseClientJson mcVersion
    return (mainClass clientJson)

getLibraries :: MCVersion -> AppStateIO [String]
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
