module Minecraft.Server.MinecraftServer (runMinecraftServer) where

import           AppState
import           CrossPlatform                (javaExecName)
import           ProcessIO

import           Data.Minecraft.MCServerBrand (getMCServerExecutableName)
import           System.FilePath              ((</>))
import           System.Process               (ProcessHandle)

runMinecraftServer :: AppStateIO ProcessHandle
runMinecraftServer = do
    workingDir    <- getWorkingDir
    jvmOptions    <- getServerJvmOptions
    serverVersion <- getServerVersion
    serverBrand   <- getMCServerBrand

    let serverJar = workingDir </> getMCServerExecutableName serverBrand serverVersion

    execProcessQuiet javaExecName (jvmOptions ++ ["-jar", serverJar]) workingDir
        "Failed to execute java that was to run a Minecraft server"
