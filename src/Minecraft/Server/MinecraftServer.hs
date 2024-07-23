module Minecraft.Server.MinecraftServer (runMinecraftServer) where

import           Imports

import           AppState
import           CrossPlatform                (javaExecName)
import           ProcessIO

import           Data.Minecraft.MCServerBrand (getMCServerExecutableName)
import           System.Process               (ProcessHandle)

runMinecraftServer :: AppStateIO ProcessHandle
runMinecraftServer = do
    workingDir    <- getWorkingDir
    jvmOptions    <- getServerJvmOptions
    serverVersion <- getServerVersion
    serverBrand   <- getMCServerBrand
    serverPort    <- getServerPort

    let serverJar = workingDir </> getMCServerExecutableName serverBrand serverVersion

    handle <- execProcessQuiet javaExecName (jvmOptions ++ ["-jar", serverJar]) workingDir
        "Failed to execute java that was to run the Minecraft server: %s."

    putStrLn' $
        printf "%s server is listening to the port %d of localhost." (show serverBrand) serverPort

    return handle
