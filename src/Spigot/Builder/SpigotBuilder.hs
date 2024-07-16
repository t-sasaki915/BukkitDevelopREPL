module Spigot.Builder.SpigotBuilder (buildSpigot) where

import           AppState
import           FileIO
import           ProcessIO
import           Spigot.Builder.Resource (buildToolsUrl)

makeNecessaryDirectories :: AppStateIO ()
makeNecessaryDirectories = do
    buildDir <- getBuildDir

    makeDirectory buildDir $
        "Failed to make a directory '" ++ buildDir ++ "'"

downloadBuildTools :: AppStateIO ()
downloadBuildTools = do
    downloadPath <- getBuildToolsPath
    buildDir     <- getBuildDir

    execProcess "curl.exe" ["-L", "-o", downloadPath, buildToolsUrl] buildDir
        "Failed to execute curl.exe that was to download BuildTools" >>=
            expectExitSuccess
                "Failed to download BuildTools"

useBuildTools :: AppStateIO ()
useBuildTools = do
    buildToolsPath <- getBuildToolsPath
    serverVersion  <- getServerVersion
    buildDir       <- getBuildDir

    execProcess "java.exe" ["-jar", buildToolsPath, "--rev", show serverVersion] buildDir
        "Failed to execute java.exe that was to build a Spigot server" >>=
            expectExitSuccess
                "Failed to build a Spigot server"

adoptServerJar :: AppStateIO ()
adoptServerJar = do
    copyPath      <- getServerJarPath
    serverJarPath <- getTemporaryServerJarPath

    copyFile' serverJarPath copyPath $
        "Failed to copy '" ++ serverJarPath ++ "' to '" ++ copyPath ++ "'"

buildSpigot :: AppStateIO ()
buildSpigot = do
    makeNecessaryDirectories
    downloadBuildTools
    useBuildTools
    adoptServerJar
