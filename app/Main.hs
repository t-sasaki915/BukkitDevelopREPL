module Main (main) where

import AppOptions
import FileIO
import MinecraftClient
import PluginInstaller
import ProcessIO
import SpigotServer
import SpigotServerSetup

import Control.Monad (when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Control.Monad.Trans.State.Strict (StateT, runStateT, get)
import Data.Version (showVersion)
import Options.Applicative
import System.Directory (getCurrentDirectory, getHomeDirectory)
import System.Exit (exitSuccess)
import System.Process (waitForProcess)

import Paths_spigot_debugger_launcher (version)

program :: ExceptT String (StateT AppOptions IO) ()
program = do
    appOptions <- lift get
    let workDir    = workingDir appOptions
        tmpWorkDir = tempWorkDir appOptions
        clientJar  = minecraftClientJarFile appOptions
        serverJar  = spigotServerJarFile appOptions
        serverOnly = noClient appOptions

    makeDirectory workDir
    execProcess "java.exe" ["-version"] workDir >>= expectExitSuccess
    verifyFileExistence clientJar $
        "The launcher could not find Minecraft client executable: " ++ clientJar

    checkFileExistence serverJar >>= \case
        True  -> do
            instalPlugins

            clientProcess <- runMinecraftClient
            when serverOnly (terminate clientProcess)
            
            serverProcess <- runSpigotServer
            _             <- lift $ lift $ waitForProcess serverProcess

            terminate serverProcess
            terminate clientProcess

        False -> do
            execProcess "git.exe" ["--version"] workDir >>= expectExitSuccess
            lift $ lift $ putStrLn "No Spigot server has found. Building..."

            makeDirectory tmpWorkDir
            downloadBuildTools
            buildSpigot
            setupSpigotServer

            lift $ lift exitSuccess

main :: IO ()
main = do
    currentDir  <- getCurrentDirectory
    homeDir     <- getHomeDirectory
    appOptions  <- customExecParser (prefs disambiguate)
                    ( info (helper <*> appOptionsParser currentDir homeDir)
                        ( fullDesc
                            <> header ("spigot-debugger-launcher " ++ showVersion version ++ " by TSasaki")
                        )
                    )

    (result, _) <- runStateT (runExceptT program) appOptions
    either putStrLn (const (return ())) result
