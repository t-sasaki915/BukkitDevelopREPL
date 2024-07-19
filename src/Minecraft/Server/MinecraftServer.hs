module Minecraft.Server.MinecraftServer (runMinecraftServer) where

import           AppState
import           Minecraft.Server.ServerBrand (getServerExecutableName)
import           ProcessIO

import           System.FilePath              ((</>))
import           System.Process               (ProcessHandle)

runMinecraftServer :: AppStateIO ProcessHandle
runMinecraftServer = do
    workingDir    <- getWorkingDir
    jvmOptions    <- getServerJvmOptions
    serverVersion <- getServerVersion
    serverBrand   <- getServerBrand

    let serverJar = workingDir </> getServerExecutableName serverBrand serverVersion

    execProcessQuiet "java.exe" (jvmOptions ++ ["-jar", serverJar]) workingDir
        "Failed to execute java.exe that was to run a Minecraft server"
