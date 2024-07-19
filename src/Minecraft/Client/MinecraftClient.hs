module Minecraft.Client.MinecraftClient (spawnMinecraftClient) where

import           AppState
import           CrossPlatform                       (javaExecName,
                                                      javaLibrarySeparator)
import           FileIO
import           Minecraft.Client.ClientJsonAnalyser as ClientJson
import           Minecraft.MinecraftVersion          (MinecraftVersion (..))
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
    binDir      <- getMinecraftBinDir

    assetIndex  <- ClientJson.getAssetIndex clientVersion
    libraries   <- ClientJson.getLibraries clientVersion
    mainClass   <- ClientJson.getMainClass clientVersion

    nativeDirs  <- directoryContents binDir $
        "Failed to enumerate the contents of '" ++ binDir ++ "'"

    let nativeOption = "-Djava.library.path=" ++ intercalate javaLibrarySeparator nativeDirs
        clientJar = versionsDir </> show clientVersion </> (show clientVersion ++ ".jar")
        libs = map (libsDir </>) libraries ++ [clientJar]
        assetsOptions
            | clientVersion < MinecraftVersion 1 7 3 =
                [ "--assetsDir"
                , assetsDir </> "virtual" </> "legacy"
                ]
            | otherwise =
                [ "--assetsDir"
                , assetsDir
                , "--assetIndex"
                , assetIndex
                ]
        clientOptions =
            [ "-cp"
            , intercalate javaLibrarySeparator libs
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
            ] ++ assetsOptions

    execProcessQuiet javaExecName (jvmOptions ++ [nativeOption] ++ clientOptions) workDir
        "Failed to execute java that was to run a Minecraft client"

spawnMinecraftClient :: MinecraftVersion -> String -> AppStateIO ProcessHandle
spawnMinecraftClient clientVersion clientUsername = do
    makeNecessaryDirectories
    createClientProcess clientVersion clientUsername
