module Minecraft.Server.Spigot.SpigotServer (runSpigotServer) where

import           AppState
import           ProcessIO

import           System.Process (ProcessHandle)

runSpigotServer :: AppStateIO ProcessHandle
runSpigotServer = do
    workingDir <- getWorkingDir
    serverJar  <- getServerJarPath
    serverOpts <- getServerJvmOptions

    execProcessNewWindow "java.exe" ("-jar" : serverJar : serverOpts) workingDir
        "Failed to execute java.exe that was to run a Spigot server"
