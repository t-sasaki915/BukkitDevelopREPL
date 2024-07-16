#!/usr/bin/env cabal
{- cabal:
build-depends: base >=4.7 && <5
             , aeson ==2.1.2.1
             , bytestring ==0.11.5.3
             , text ==2.0.2
-}

-- Usage: cabal run ClientJsonScript.hs <assetIndex | libraries> <Path to client.json>

{-# LANGUAGE OverloadedStrings #-}

import           Data.Aeson
import qualified Data.ByteString    as BS
import           Data.Functor       ((<&>))
import           Data.Text          (Text)
import           System.Environment (getArgs)

data Features = Features
    { isDemoUser          :: Maybe Bool
    , hasCustomResolution :: Maybe Bool
    }
    deriving Show

data OS = OS
    { name    :: Maybe Text
    , version :: Maybe Text
    , arch    :: Maybe Text
    }
    deriving Show

data Rule = Rule
    { action   :: Text
    , features :: Maybe Features
    , os       :: Maybe OS
    }
    deriving Show

data ArgumentValue = SingleValue Text
                   | MultiValue [Text]
                   deriving Show

data Argument = CommonArgument Text
              | ConditionalArgument [Rule] ArgumentValue
              deriving Show

data Arguments = Arguments
    { game :: [Argument]
    , jvm  :: [Argument]
    }
    deriving Show

data AssetIndex = AssetIndex
    { assetIndexId   :: Text
    , assetIndexSha1 :: Text
    , assetIndexSize :: Integer
    , totalSize      :: Integer
    , assetIndexUrl  :: Text
    }
    deriving Show

data Download = Download
    { downloadSha1 :: Text
    , downloadSize :: Integer
    , downloadUrl  :: Text
    }
    deriving Show

data Downloads = Downloads
    { client         :: Download
    , clientMappings :: Maybe Download
    , server         :: Download
    , serverMappings :: Maybe Download
    }
    deriving Show

data JavaVersion = JavaVersion
    { component    :: Text
    , majorVersion :: Int
    }
    deriving Show

data ClientLoggingFile = ClientLoggingFile
    { clientLoggingFileId   :: Text
    , clientLoggingFileSha1 :: Text
    , clientLoggingFileSize :: Integer
    , clientLoggingFileUrl  :: Text
    }
    deriving Show

data ClientLogging = ClientLogging
    { argument          :: Text
    , file              :: ClientLoggingFile
    , clientLoggingType :: Text
    }
    deriving Show

newtype Logging = Logging
    { clientLogging :: ClientLogging
    }
    deriving Show

data Artefact = Artefact
    { artefactPath :: Text
    , artefactSha1 :: Text
    , artefactSize :: Integer
    , artefactUrl  :: Text
    }
    deriving Show

data Classifiers = Classifiers
    { nativesLinux   :: Maybe Artefact
    , nativesOSX     :: Maybe Artefact
    , nativesWindows :: Maybe Artefact
    }
    deriving Show

data LibraryDownloads = LibraryDownloads
    { artefact    :: Artefact
    , classifiers :: Maybe Classifiers
    }
    deriving Show

data Natives = Natives
    { linux   :: Maybe Text
    , osx     :: Maybe Text
    , windows :: Maybe Text
    }
    deriving Show

newtype Extract = Extract
    { exclude :: [Text]
    }
    deriving Show

data Library = Library
    { libraryDownloads :: LibraryDownloads
    , libraryName      :: Text
    , natives          :: Maybe Natives
    , extract          :: Maybe Extract
    , rules            :: Maybe [Rule]
    }
    deriving Show

data ClientJSON = ClientJSON
    { arguments              :: Maybe Arguments
    , assetIndex             :: AssetIndex
    , assets                 :: Text
    , complianceLevel        :: Int
    , downloads              :: Downloads
    , clientJSONId           :: Text
    , javaVersion            :: JavaVersion
    , libraries              :: [Library]
    , logging                :: Logging
    , mainClass              :: Text
    , minecraftArguments     :: Maybe Text
    , minimumLauncherVersion :: Int
    , releaseTime            :: Text
    , time                   :: Text
    , clientType             :: Text
    }
    deriving Show

instance FromJSON Features where
    parseJSON (Object m) =
        Features
            <$> (m .:? "is_demo_user")
            <*> (m .:? "has_custom_resolution")

    parseJSON x = fail ("Parse failure: " ++ show x)

instance FromJSON OS where
    parseJSON (Object m) =
        OS
            <$> (m .:? "name")
            <*> (m .:? "version")
            <*> (m .:? "arch")

    parseJSON x = fail ("Parse failure: " ++ show x)

instance FromJSON Rule where
    parseJSON (Object m) =
        Rule
            <$> (m .:  "action")
            <*> (m .:? "features")
            <*> (m .:? "os")

    parseJSON x = fail ("Parse failure: " ++ show x)

instance FromJSON ArgumentValue where
    parseJSON (String s) = pure (SingleValue s)
    parseJSON (Array xs) = parseJSON (Array xs) <&> MultiValue

    parseJSON x          = fail ("Parse failure: " ++ show x)

instance FromJSON Argument where
    parseJSON (String s) = pure (CommonArgument s)
    parseJSON (Object m) =
        ConditionalArgument
            <$> (m .: "rules")
            <*> (m .: "value")

    parseJSON x = fail ("Parse failure: " ++ show x)

instance FromJSON Arguments where
    parseJSON (Object m) =
        Arguments
            <$> (m .: "game")
            <*> (m .: "jvm")

    parseJSON x = fail ("Parse failure: " ++ show x)

instance FromJSON AssetIndex where
    parseJSON (Object m) =
        AssetIndex
            <$> (m .: "id")
            <*> (m .: "sha1")
            <*> (m .: "size")
            <*> (m .: "totalSize")
            <*> (m .: "url")

    parseJSON x = fail ("Parse failure: " ++ show x)

instance FromJSON Download where
    parseJSON (Object m) =
        Download
            <$> (m .: "sha1")
            <*> (m .: "size")
            <*> (m .: "url")

    parseJSON x = fail ("Parse failure: " ++ show x)

instance FromJSON Downloads where
    parseJSON (Object m) =
        Downloads
            <$> (m .:  "client")
            <*> (m .:? "client_mappings")
            <*> (m .:  "server")
            <*> (m .:? "server_mappings")

    parseJSON x = fail ("Parse failure: " ++ show x)

instance FromJSON JavaVersion where
    parseJSON (Object m) =
        JavaVersion
            <$> (m .: "component")
            <*> (m .: "majorVersion")

    parseJSON x = fail ("Parse failure: " ++ show x)

instance FromJSON ClientLoggingFile where
    parseJSON (Object m) =
        ClientLoggingFile
            <$> (m .: "id")
            <*> (m .: "sha1")
            <*> (m .: "size")
            <*> (m .: "url")

    parseJSON x = fail ("Parse failure: " ++ show x)

instance FromJSON ClientLogging where
    parseJSON (Object m) =
        ClientLogging
            <$> (m .: "argument")
            <*> (m .: "file")
            <*> (m .: "type")

    parseJSON x = fail ("Parse failure: " ++ show x)

instance FromJSON Logging where
    parseJSON (Object m) =
        Logging
            <$> (m .: "client")

    parseJSON x = fail ("Parse failure: " ++ show x)

instance FromJSON Artefact where
    parseJSON (Object m) =
        Artefact
            <$> (m .: "path")
            <*> (m .: "sha1")
            <*> (m .: "size")
            <*> (m .: "url")

    parseJSON x = fail ("Parse failure: " ++ show x)

instance FromJSON Classifiers where
    parseJSON (Object m) =
        Classifiers
            <$> (m .:? "natives-linux")
            <*> (m .:? "natives-osx")
            <*> (m .:? "natives-windows")

    parseJSON x = fail ("Parse failure: " ++ show x)

instance FromJSON LibraryDownloads where
    parseJSON (Object m) =
        LibraryDownloads
            <$> (m .:  "artifact")
            <*> (m .:? "classifiers")

    parseJSON x = fail ("Parse failure: " ++ show x)

instance FromJSON Natives where
    parseJSON (Object m) =
        Natives
            <$> (m .:? "linux")
            <*> (m .:? "osx")
            <*> (m .:? "windows")

    parseJSON x = fail ("Parse failure: " ++ show x)

instance FromJSON Extract where
    parseJSON (Object m) =
        Extract
            <$> (m .: "exclude")

    parseJSON x = fail ("Parse failure: " ++ show x)

instance FromJSON Library where
    parseJSON (Object m) =
        Library
            <$> (m .:  "downloads")
            <*> (m .:  "name")
            <*> (m .:? "natives")
            <*> (m .:? "extract")
            <*> (m .:? "rules")

    parseJSON x = fail ("Parse failure: " ++ show x)

instance FromJSON ClientJSON where
    parseJSON (Object m) =
        ClientJSON
            <$> (m .:? "arguments")
            <*> (m .:  "assetIndex")
            <*> (m .:  "assets")
            <*> (m .:  "complianceLevel")
            <*> (m .:  "downloads")
            <*> (m .:  "id")
            <*> (m .:  "javaVersion")
            <*> (m .:  "libraries")
            <*> (m .:  "logging")
            <*> (m .:  "mainClass")
            <*> (m .:? "minecraftArguments")
            <*> (m .:  "minimumLauncherVersion")
            <*> (m .:  "releaseTime")
            <*> (m .:  "time")
            <*> (m .:  "type")

    parseJSON x = fail ("Parse Failure: " ++ show x)

program :: [String] -> IO ()
program ["assetIndex", filePath] = do
    rawJSON    <- BS.readFile filePath
    clientJSON <- throwDecode (BS.fromStrict rawJSON) :: IO ClientJSON

    print clientJSON

program ["libraries", filePath]  = putStrLn ("libraries " ++ filePath)

program _                        = putStrLn "Invalid usage."

main :: IO ()
main = do
    args <- getArgs
    program args
