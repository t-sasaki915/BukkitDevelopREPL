module Data.Minecraft.MCServerBrand (MCServerBrand(..), getMCServerExecutableName) where

import           Data.Minecraft.MCVersion (MCVersion)
import           Data.Yaml                (FromJSON (..), ToJSON (..),
                                           Value (..))
import           Text.Printf              (printf)

data MCServerBrand = Spigot | Paper deriving Show

instance FromJSON MCServerBrand where
    parseJSON (String "Spigot") = pure Spigot
    parseJSON (String "Paper")  = pure Paper
    parseJSON x                 = fail (printf "Unrecognisable server brand '%s'." (show x))

instance ToJSON MCServerBrand where
    toJSON Spigot = String "Spigot"
    toJSON Paper  = String "Paper"

getMCServerExecutableName :: MCServerBrand -> MCVersion -> String
getMCServerExecutableName brand version =
    printf "%s-%s.jar" (show brand) (show version)
