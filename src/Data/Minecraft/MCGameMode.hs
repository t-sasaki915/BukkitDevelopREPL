module Data.Minecraft.MCGameMode (MCGameMode(..)) where

import           Imports

import           Data.Yaml (FromJSON (..), ToJSON (..), Value (..))

data MCGameMode = Survival | Creative | Adventure | Spectator

instance Show MCGameMode where
    show Survival  = "survival"
    show Creative  = "creative"
    show Adventure = "adventure"
    show Spectator = "spectator"

instance FromJSON MCGameMode where
    parseJSON (String "survival")  = pure Survival
    parseJSON (String "creative")  = pure Creative
    parseJSON (String "adventure") = pure Adventure
    parseJSON (String "spectator") = pure Spectator
    parseJSON (Number 0)           = pure Survival
    parseJSON (Number 1)           = pure Creative
    parseJSON (Number 2)           = pure Adventure
    parseJSON (Number 3)           = pure Spectator
    parseJSON x                    = fail (printf "Unrecognisable gamemode '%s'." (show x))

instance ToJSON MCGameMode where
    toJSON Survival  = String "survival"
    toJSON Creative  = String "creative"
    toJSON Adventure = String "adventure"
    toJSON Spectator = String "spectator"
