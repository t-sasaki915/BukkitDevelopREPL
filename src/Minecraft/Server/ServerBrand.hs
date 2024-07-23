module Minecraft.Server.ServerBrand (ServerBrand(..), getServerExecutableName) where

import           Data.Minecraft.MCVersion (MCVersion)
import           Data.Yaml                (FromJSON (..), Value (..))

data ServerBrand = Spigot | Paper deriving Show

instance FromJSON ServerBrand where
    parseJSON (String "Spigot") = pure Spigot
    parseJSON (String "Paper")  = pure Paper
    parseJSON _                 = fail "Unrecognisable server brand"

getServerExecutableName :: ServerBrand -> MCVersion -> String
getServerExecutableName brand version =
    show brand ++ "-" ++ show version ++ ".jar"
