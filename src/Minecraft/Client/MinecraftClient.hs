module Minecraft.Client.MinecraftClient (spawnMinecraftClient) where

import           AppState
import           FileIO
import           Minecraft.Client.ClientJsonAnalyser as ClientJson
import           Minecraft.MinecraftVersion          (MinecraftVersion)
import           ProcessIO

import           Data.List                           (intercalate)
import           System.FilePath                     ((</>))
import           System.Process                      (ProcessHandle)

makeNecessaryDirectories :: AppStateIO ()
makeNecessaryDirectories = do
    workDir <- getClientWorkingDir

    makeDirectory workDir $
        "Failed to make a directory '" ++ workDir ++ "'"

createClientProcess :: MinecraftVersion -> String -> AppStateIO ProcessHandle
createClientProcess clientVersion clientUsername = do
    workDir     <- getClientWorkingDir
    jvmOptions  <- getClientJvmOptions
    assetsDir   <- getMinecraftAssetsDir
    libsDir     <- getMinecraftLibrariesDir
    versionsDir <- getMinecraftVersionsDir

    assetIndex  <- ClientJson.getAssetIndex clientVersion
    libraries   <- ClientJson.getLibraries clientVersion
    mainClass   <- ClientJson.getMainClass clientVersion

    let clientJar = versionsDir </> show clientVersion </> (show clientVersion ++ ".jar")
        libs = map (libsDir </>) libraries ++ [clientJar]
        clientOptions =
            [ "-cp"
            , intercalate ";" libs
            , mainClass
            , "--username"
            , clientUsername
            , "--version"
            , show clientVersion
            , "--accessToken"
            , "0"
            , "--userProperties"
            , "{}"
            , "--gameDir"
            , workDir
            , "--assetsDir"
            , assetsDir
            , "--assetIndex"
            , assetIndex
            ]

    execProcessNewWindow "java.exe" (jvmOptions ++ clientOptions) workDir
        "Failed to execute java.exe that was to run a Minecraft client"

spawnMinecraftClient :: MinecraftVersion -> String -> AppStateIO ProcessHandle
spawnMinecraftClient clientVersion clientUsername = do
    makeNecessaryDirectories
    createClientProcess clientVersion clientUsername
