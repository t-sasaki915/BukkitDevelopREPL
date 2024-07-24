module Minecraft.Server.Spigot.SpigotSetup (setupSpigot) where

import           Imports

import           AppState
import           CrossPlatform                (curlExecName, javaExecName)
import           ProcessIO

import           Data.Minecraft.MCServerBrand (MCServerBrand (Spigot),
                                               getMCServerExecutableName)

makeNecessaryDirectories :: AppStateIO ()
makeNecessaryDirectories = do
    buildDir <- getBuildDir
    lift (createDirectoryIfMissing True buildDir)

downloadBuildTools :: AppStateIO ()
downloadBuildTools = do
    buildDir <- getBuildDir

    putStrLn' "Downloading BuildTools..."

    let buildToolsUrl = "https://hub.spigotmc.org/jenkins/job/BuildTools/lastSuccessfulBuild/artifact/target/BuildTools.jar"
        downloadPath  = buildDir </> "BuildTools.jar"

    execProcessQuiet curlExecName ["-L", "-o", downloadPath, buildToolsUrl] buildDir >>=
        expectExitSuccess (printf "Failed to download BuildTools '%s': %%s." buildToolsUrl)

useBuildTools :: AppStateIO ()
useBuildTools = do
    serverVersion  <- getServerVersion
    buildDir       <- getBuildDir

    putStrLn' "Building a Spigot Server... This will take some minutes."

    let buildToolsPath = buildDir </> "BuildTools.jar"

    execProcess javaExecName ["-jar", buildToolsPath, "--rev", show serverVersion] buildDir >>=
        expectExitSuccess "Failed to build a Spigot server: %s."

adoptServerJar :: AppStateIO ()
adoptServerJar = do
    workingDir    <- getWorkingDir
    buildDir      <- getBuildDir
    serverVersion <- getServerVersion

    let copyPath      = workingDir </> getMCServerExecutableName Spigot serverVersion
        serverJarPath = buildDir </> printf "spigot-%s.jar" (show serverVersion)

    lift (copyFile serverJarPath copyPath)

setupSpigot :: AppStateIO ()
setupSpigot = do
    makeNecessaryDirectories
    downloadBuildTools
    useBuildTools
    adoptServerJar
