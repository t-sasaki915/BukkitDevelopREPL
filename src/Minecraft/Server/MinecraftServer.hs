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
        serverOpts =
            [ "-jar"
            , serverJar
            , "nogui"
            ]

    execProcessNewWindow "java.exe" (jvmOptions ++ serverOpts) workingDir
        "Failed to execute java.exe that was to run a Minecraft server"
