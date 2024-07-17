{-# LANGUAGE TemplateHaskell #-}

module Config.Resource (defaultConfigFile) where

import           Data.ByteString (ByteString)
import           Data.FileEmbed  (embedFile)

defaultConfigFile :: ByteString
defaultConfigFile = $(embedFile ".BukkitDevelopREPL.yaml")
