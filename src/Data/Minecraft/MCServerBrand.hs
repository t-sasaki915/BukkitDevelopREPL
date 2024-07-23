module Data.Minecraft.MCServerBrand (MCServerBrand(..), getMCServerExecutableName) where

import           Data.Minecraft.MCVersion (MCVersion)
import           Data.Yaml                (FromJSON (..), Value (..))

data MCServerBrand = Spigot | Paper deriving Show

instance FromJSON MCServerBrand where
    parseJSON (String "Spigot") = pure Spigot
    parseJSON (String "Paper")  = pure Paper
    parseJSON _                 = fail "Unrecognisable server brand"

getMCServerExecutableName :: MCServerBrand -> MCVersion -> String
getMCServerExecutableName brand version =
    show brand ++ "-" ++ show version ++ ".jar"
