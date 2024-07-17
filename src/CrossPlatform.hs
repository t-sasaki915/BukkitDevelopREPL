{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

module CrossPlatform (OSType(..), currentOSType) where

import           Data.Aeson (FromJSON (parseJSON), Value (String))

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
