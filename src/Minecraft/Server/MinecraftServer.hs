module Minecraft.Server.MinecraftServer (runMinecraftServer) where

import           AppState
import           Minecraft.Server.ServerBrand (getServerExecutableName)
import           ProcessIO

import           System.FilePath              ((</>))
import           System.Process               (ProcessHandle)

runMinecraftServer :: AppStateIO ProcessHandle
runMinecraftServer = do
    workingDir    <- getWorkingDir
    serverOpts    <- getServerJvmOptions
    serverVersion <- getServerVersion
    serverBrand   <- getServerBrand

    let serverJar = workingDir </> getServerExecutableName serverBrand serverVersion

    execProcessNewWindow "java.exe" ("-jar" : serverJar : serverOpts) workingDir
        "Failed to execute java.exe that was to run a Minecraft server"
