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
    serverPort    <- getServerPort

    let serverJar = workingDir </> getMCServerExecutableName serverBrand serverVersion

    handle <- execProcessQuiet javaExecName (jvmOptions ++ ["-jar", serverJar]) workingDir
        "Failed to execute java that was to run the Minecraft server"

    putStrLn' $
        show serverBrand ++ " server is listening to the port " ++ show serverPort ++ " of localhost."

    return handle
