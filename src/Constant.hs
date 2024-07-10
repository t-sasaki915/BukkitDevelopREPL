{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Constant where

import System.FilePath ((</>))

workingDirPath :: String
workingDirPath = "." </> "run"

workingTempDirPath :: String
workingTempDirPath = workingDirPath </> "temp"

minecraftVersionsDirPath :: String -> String
minecraftVersionsDirPath home = home </> "AppData" </> "Roaming" </> ".minecraft" </> "versions"

spigotServerFileName :: String
spigotServerFileName = "spigot-" ++ minecraftVersion ++ ".jar"

minecraftVersion :: String
minecraftVersion = "1.20.1"

buildToolsUrl :: String
buildToolsUrl = "https://hub.spigotmc.org/jenkins/job/BuildTools/lastSuccessfulBuild/artifact/target/BuildTools.jar"
