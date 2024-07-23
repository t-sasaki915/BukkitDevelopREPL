module Data.Minecraft.MCServerBrand (MCServerBrand(..), getMCServerExecutableName) where

import           Data.Minecraft.MCVersion (MCVersion)
import           Data.Yaml                (FromJSON (..), ToJSON (..),
                                           Value (..))

data MCServerBrand = Spigot | Paper deriving Show

instance FromJSON MCServerBrand where
    parseJSON (String "Spigot") = pure Spigot
    parseJSON (String "Paper")  = pure Paper
    parseJSON _                 = fail "Unrecognisable server brand"

instance ToJSON MCServerBrand where
    toJSON Spigot = String "Spigot"
    toJSON Paper  = String "Paper"

getMCServerExecutableName :: MCServerBrand -> MCVersion -> String
getMCServerExecutableName brand version =
    show brand ++ "-" ++ show version ++ ".jar"
