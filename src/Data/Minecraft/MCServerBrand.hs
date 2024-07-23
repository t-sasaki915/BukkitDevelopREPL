module Data.Minecraft.MCServerBrand (MCServerBrand(..), getMCServerExecutableName) where

import           Imports

import           Data.Minecraft.MCVersion (MCVersion)
import           Data.Yaml                (FromJSON (..), ToJSON (..),
                                           Value (..))

data MCServerBrand = Spigot | Paper deriving Show

instance FromJSON MCServerBrand where
    parseJSON (String "Spigot") = pure Spigot
    parseJSON (String "Paper")  = pure Paper
    parseJSON x                 = fail (printf "Unrecognisable server brand '%s'." (show x))

instance ToJSON MCServerBrand where
    toJSON Spigot = String "Spigot"
    toJSON Paper  = String "Paper"

getMCServerExecutableName :: MCServerBrand -> MCVersion -> String
getMCServerExecutableName brand mcVersion =
    printf "%s-%s.jar" (show brand) (show mcVersion)
