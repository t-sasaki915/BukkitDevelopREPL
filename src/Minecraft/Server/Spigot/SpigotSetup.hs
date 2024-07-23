module Minecraft.Server.Spigot.SpigotSetup (setupSpigot) where

import           AppState
import           CrossPlatform                (curlExecName, javaExecName)
import           FileIO
import           ProcessIO

import           Data.Minecraft.MCServerBrand (MCServerBrand (Spigot),
                                               getMCServerExecutableName)
import           System.FilePath              ((</>))
import           Text.Printf                  (printf)

makeNecessaryDirectories :: AppStateIO ()
makeNecessaryDirectories = do
    buildDir <- getBuildDir

    makeDirectory buildDir $
        printf "Failed to make a directory '%s': %%s." buildDir

downloadBuildTools :: AppStateIO ()
downloadBuildTools = do
    buildDir <- getBuildDir

    putStrLn' "Downloading BuildTools..."

    let buildToolsUrl = "https://hub.spigotmc.org/jenkins/job/BuildTools/lastSuccessfulBuild/artifact/target/BuildTools.jar"
        downloadPath  = buildDir </> "BuildTools.jar"

    execProcess curlExecName ["-L", "-o", downloadPath, buildToolsUrl] buildDir
        (printf "Failed to execute curl that was to download BuildTools '%s': %%s." buildToolsUrl) >>=
            expectExitSuccess
                (printf "Failed to download BuildTools '%s': %%s." buildToolsUrl)

useBuildTools :: AppStateIO ()
useBuildTools = do
    serverVersion  <- getServerVersion
    buildDir       <- getBuildDir

    putStrLn' "Building a Spigot Server... This will take some minutes."

    let buildToolsPath = buildDir </> "BuildTools.jar"

    execProcess javaExecName ["-jar", buildToolsPath, "--rev", show serverVersion] buildDir
        "Failed to execute java that was to build a Spigot server: %s." >>=
            expectExitSuccess
                "Failed to build a Spigot server: %s."

adoptServerJar :: AppStateIO ()
adoptServerJar = do
    workingDir    <- getWorkingDir
    buildDir      <- getBuildDir
    serverVersion <- getServerVersion

    let copyPath      = workingDir </> getMCServerExecutableName Spigot serverVersion
        serverJarPath = buildDir </> printf "spigot-%s.jar" (show serverVersion)

    copyFile' serverJarPath copyPath $
        printf "Failed to copy a file '%s' to '%s': %%s" serverJarPath copyPath

setupSpigot :: AppStateIO ()
setupSpigot = do
    makeNecessaryDirectories
    downloadBuildTools
    useBuildTools
    adoptServerJar
