module Main (main) where

import AppOptions
import FileIO
import MinecraftClient
import PluginInstaller
import ProcessIO
import SpigotServer
import SpigotServerSetup

import Control.Monad (when, unless)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Control.Monad.Trans.State.Strict (StateT, runStateT, get)
import Data.Version (showVersion)
import Options.Applicative
import System.Directory (getCurrentDirectory, getHomeDirectory)
import System.Exit (exitSuccess, exitFailure)
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

    makeDirectory workDir "Failed to make the working directory"
    execProcess "java.exe" ["-version"] workDir
        "This program requires java.exe but could not find it" >>=
            expectExitSuccess
                "Failed to check the version of java.exe"
    checkFileExistence clientJar
        ("Failed to check the existence of Minecraft client executable '" ++ clientJar ++ "'") >>= \exists ->
            unless exists $ throwE ("Could not find Minecraft client executable '" ++ clientJar ++ "'")

    checkFileExistence serverJar
        ("Failed to check the existence of Spigot server executable '" ++ serverJar ++ "'") >>= \case
            True  -> do
                instalPlugins

                clientProcess <- runMinecraftClient
                when serverOnly (terminate clientProcess)
            
                serverProcess <- runSpigotServer
                _             <- lift $ lift $ waitForProcess serverProcess

                terminate serverProcess
                terminate clientProcess

            False -> do
                execProcess "git.exe" ["--version"] workDir
                    "This program requires git.exe but could not find it" >>=
                        expectExitSuccess
                            "Failed to check the version of git.exe"
                lift $ lift $ putStrLn "No Spigot server has found. Building..."

                makeDirectory tmpWorkDir "Failed to make the temporary working directory"
                downloadBuildTools
                buildSpigot
                setupSpigotServer

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
    case result of
        Right () -> exitSuccess
        Left err -> putStrLn err >> exitFailure
