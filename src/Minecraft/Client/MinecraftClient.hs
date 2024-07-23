module Minecraft.Client.MinecraftClient (spawnMinecraftClient) where

import           Imports

import           AppState
import           CrossPlatform                       (javaExecName,
                                                      javaLibrarySeparator)
import           FileIO
import           Minecraft.Client.ClientJsonAnalyser as ClientJson
import           ProcessIO

import           Data.List                           (intercalate)
import           Data.Minecraft.MCVersion            (MCVersion (..))
import           System.Process                      (ProcessHandle)

makeNecessaryDirectories :: AppStateIO ()
makeNecessaryDirectories = do
    workDir <- getClientWorkingDir

    makeDirectory workDir $
        printf "Failed to make a directory '%s': %%s." workDir

createClientProcess :: MCVersion -> String -> AppStateIO ProcessHandle
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
        printf "Failed to enumerate the contents of '%s': %%s." binDir

    let nativeOption = printf "-Djava.library.path=%s" (intercalate javaLibrarySeparator nativeDirs)
        clientJar = versionsDir </> show clientVersion </> printf "%s.jar" (show clientVersion)
        libs = map (libsDir </>) libraries ++ [clientJar]
        assetsOptions
            | clientVersion < MCVersion 1 7 3 =
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
        "Failed to execute java that was to run a Minecraft client: %s."

spawnMinecraftClient :: MCVersion -> String -> AppStateIO ProcessHandle
spawnMinecraftClient clientVersion clientUsername = do
    makeNecessaryDirectories
    createClientProcess clientVersion clientUsername
