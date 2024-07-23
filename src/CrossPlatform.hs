{-# LANGUAGE CPP #-}

module CrossPlatform
    ( OSType(..)
    , currentOSType
    , javaLibrarySeparator
    , defaultMinecraftDir
    , javaExecName
    , curlExecName
    ) where

import           Data.Aeson      (FromJSON (parseJSON), Value (String))
import           System.FilePath ((</>))

data OSType = Linux | OSX | Windows deriving (Show, Eq)

currentOSType :: OSType
#ifdef mingw32_HOST_OS
currentOSType = Windows
#endif
#ifdef linux_HOST_OS
currentOSType = Linux
#endif
#ifdef darwin_HOST_OS
currentOSType = OSX
#endif

instance FromJSON OSType where
    parseJSON (String "linux")   = pure Linux
    parseJSON (String "osx")     = pure OSX
    parseJSON (String "windows") = pure Windows
    parseJSON x                  = fail ("Unrecognisable os type: " ++ show x)

javaLibrarySeparator :: String
javaLibrarySeparator = case currentOSType of
    Linux   -> ":"
    OSX     -> ":"
    Windows -> ";"

defaultMinecraftDir :: FilePath -> FilePath
defaultMinecraftDir homeDir = case currentOSType of
    Linux   -> homeDir </> ".minecraft"
    OSX     -> homeDir </> "Library" </> "Application Support" </> "minecraft"
    Windows -> homeDir </> "AppData" </> "Roaming" </> ".minecraft"

javaExecName :: String
javaExecName = case currentOSType of
    Linux   -> "java"
    OSX     -> "java"
    Windows -> "java.exe"

curlExecName :: String
curlExecName = case currentOSType of
    Linux   -> "curl"
    OSX     -> "curl"
    Windows -> "curl.exe"
