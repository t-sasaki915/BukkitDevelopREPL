{-# LANGUAGE OverloadedStrings #-}

module Minecraft.Server.ServerBrand (ServerBrand(..)) where

import           Data.Yaml (FromJSON (..), Value (..))

data ServerBrand = Spigot deriving Show

instance FromJSON ServerBrand where
    parseJSON (String "Spigot") = pure Spigot
    parseJSON _                 = fail "Unrecognisable server brand"
