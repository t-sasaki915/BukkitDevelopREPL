module Minecraft.Client.MinecraftClient (spawnMinecraftClient) where

import           Imports

import           AppState
import           CrossPlatform
import           Minecraft.Client.ClientJsonAnalyser as ClientJson
import           ProcessIO

import           Data.List                           (intercalate)
import           Data.Minecraft.MCVersion            (MCVersion (..))
import           System.Directory                    (listDirectory)
import           System.Process                      (ProcessHandle)

getMinecraftAssetsDir :: AppStateIO FilePath
getMinecraftAssetsDir = getMinecraftDir <&> (</> "assets")

getMinecraftLibrariesDir :: AppStateIO FilePath
getMinecraftLibrariesDir = getMinecraftDir <&> (</> "libraries")

getMinecraftVersionsDir :: AppStateIO FilePath
getMinecraftVersionsDir = getMinecraftDir <&> (</> "versions")

getMinecraftBinDir :: AppStateIO FilePath
getMinecraftBinDir = getMinecraftDir <&> (</> "bin")

getClientWorkingDir :: AppStateIO FilePath
getClientWorkingDir = getWorkingDir <&> (</> "client")

makeNecessaryDirectories :: AppStateIO ()
makeNecessaryDirectories = do
    workDir <- getClientWorkingDir
    lift (createDirectoryIfMissing True workDir)

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

    nativeDirs  <- map (binDir </>) <$> lift (listDirectory binDir)

    let osx = currentOSType == OSX
        nativeOption = printf "-Djava.library.path=%s" (intercalate javaLibrarySeparator nativeDirs)
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

    unlessM (lift (doesFileExist clientJar)) $
        error $ printf
            "Could not find '%s'. You need to launch Minecraft %s at least once with the official Minecraft Launcher."
                clientJar (show clientVersion)

    execProcessQuiet javaExecName (jvmOptions ++ ["-XstartOnFirstThread" | osx] ++ [nativeOption] ++ clientOptions) workDir

spawnMinecraftClient :: MCVersion -> String -> AppStateIO ProcessHandle
spawnMinecraftClient clientVersion clientUsername = do
    makeNecessaryDirectories
    createClientProcess clientVersion clientUsername
