module MinecraftClient (runMinecraftClient) where

import AppOptions
import Constant
import ProcessIO

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.State.Strict (StateT, get)
import Data.List (intercalate)
import System.FilePath ((</>))
import System.Process (ProcessHandle)

runMinecraftClient :: ExceptT String (StateT AppOptions IO) ProcessHandle
runMinecraftClient = do
    appOptions <- lift get
    let workDir   = workingDir appOptions
        assetsDir = minecraftAssetsDir appOptions
        clientJar = minecraftClientJarFile appOptions
        libsDir   = minecraftLibrariesDir appOptions
        username  = minecraftClientUsername appOptions
        jvmXms    = minecraftClientXms appOptions
        jvmXmx    = minecraftClientXmx appOptions

    let libraries = map (libsDir </>) minecraftClientLibraries ++ [clientJar]
        jvmOptions =
            ["-Xms" ++ show jvmXms ++ "G", "-Xmx" ++ show jvmXmx ++ "G"]
                ++ minecraftClientJVMOptions
        clientOptions =
            [ "-cp"
            , intercalate ";" libraries
            , "net.minecraft.client.main.Main"
            , "--username"
            , username
            , "--version"
            , minecraftVersion
            , "--accessToken"
            , "0"
            , "--userProperties"
            , "{}"
            , "--gameDir"
            , workDir
            , "--assetsDir"
            , assetsDir
            , "--assetIndex"
            , show minecraftAssetIndex
            ]

    execProcess "java.exe" (jvmOptions ++ clientOptions) workDir
        "Failed to execute java.exe that was to run a Minecraft client"
