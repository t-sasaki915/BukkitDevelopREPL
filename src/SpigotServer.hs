module SpigotServer (runSpigotServer) where

import           AppOptions
import           Constant
import           ProcessIO

import           Control.Monad.Trans.Class        (lift)
import           Control.Monad.Trans.Except       (ExceptT)
import           Control.Monad.Trans.State.Strict (StateT, get)
import           System.Process                   (ProcessHandle)

runSpigotServer :: ExceptT String (StateT AppOptions IO) ProcessHandle
runSpigotServer = do
    appOptions <- lift get
    let workDir   = workingDir appOptions
        serverJar = spigotServerJarFile appOptions
        jvmXms    = minecraftServerXms appOptions
        jvmXmx    = minecraftServerXmx appOptions

    let jvmOptions =
            ["-Xms" ++ show jvmXms ++ "G", "-Xmx" ++ show jvmXmx ++ "G"]
                ++ minecraftServerJVMOptions
        serverOptions = [ "-jar", serverJar, "nogui"]

    execProcess "java.exe" (jvmOptions ++ serverOptions) workDir
        "Failed to execute java.exe that was to run a Spigot server"
