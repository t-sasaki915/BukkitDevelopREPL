{-# LANGUAGE OverloadedStrings #-}

module Minecraft.Server.ServerBrand (ServerBrand(..), getServerExecutableName) where

import           Minecraft.MinecraftVersion (MinecraftVersion)

import           Data.Yaml                  (FromJSON (..), Value (..))

data ServerBrand = Spigot | Paper deriving Show

instance FromJSON ServerBrand where
    parseJSON (String "Spigot") = pure Spigot
    parseJSON (String "Paper")  = pure Paper
    parseJSON _                 = fail "Unrecognisable server brand"

getServerExecutableName :: ServerBrand -> MinecraftVersion -> String
getServerExecutableName brand version =
    show brand ++ "-" ++ show version ++ ".jar"
