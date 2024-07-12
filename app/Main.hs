module Main (main) where

import AppOptions
import FileIO
import MinecraftClient
import PluginInstaller
import ProcessIO
import SpigotServer
import SpigotServerSetup

import Control.Monad (unless)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Control.Monad.Trans.State.Strict (StateT, runStateT, get)
import Data.Version (showVersion)
import Options.Applicative
import System.Directory (getCurrentDirectory, getHomeDirectory)
import System.Exit (exitSuccess, exitFailure, ExitCode(..))
import System.IO (hFlush, stdout)
import System.Process (waitForProcess)

import Paths_spigot_debugger_launcher (version)

program :: ExceptT String (StateT AppOptions IO) ()
program = do
    appOptions <- lift get
    let workDir    = workingDir appOptions
        tmpWorkDir = tempWorkDir appOptions
        mcRootDir  = minecraftDir appOptions
        clientJar  = minecraftClientJarFile appOptions
        serverJar  = spigotServerJarFile appOptions
        serverOnly = noClient appOptions

    execProcess "java.exe" ["-version"] workDir
        "This program requires Java but could not find it" >>=
            expectExitSuccess
                "Failed to check the version of Java"
    
    makeDirectory workDir "Failed to make the working directory"

    checkFileExistence serverJar
        ("Failed to check the existence of Spigot server executable '" ++ serverJar ++ "'") >>= \case
            True | serverOnly -> do
                instalPlugins

                serverProcess <- runSpigotServer
                
                lift (lift $ waitForProcess serverProcess) >>= \case
                    ExitSuccess     -> return ()
                    (ExitFailure n) -> throwE $
                        "The Spigot server process has finished with a code " ++ show n ++ "."

            True -> do
                checkDirectoryExistence mcRootDir
                    ("Failed to check the existence of Minecraft directory '" ++ mcRootDir ++ "'") >>=
                        \exists -> unless exists $ throwE $
                            "Could not find Minecraft directory '" ++ mcRootDir ++ "'"
                checkFileExistence clientJar
                    ("Failed to check the existence of Minecraft client executable '" ++ clientJar ++ "'") >>=
                        \exists -> unless exists $ throwE $
                            "Could not find Minecraft client executable '" ++ clientJar ++ "'"

                instalPlugins

                clientProcess <- runMinecraftClient
                serverProcess <- runSpigotServer
                exitCode      <- lift $ lift $ waitForProcess serverProcess

                terminate clientProcess

                case exitCode of
                    ExitSuccess     -> return ()
                    (ExitFailure n) -> throwE $
                        "The Spigot server process has finished with a code " ++ show n ++ "."

            False -> do
                execProcess "git.exe" ["--version"] workDir
                    "This program requires Git for Windows but could not find it" >>=
                        expectExitSuccess
                            "Failed to check the version of Git for Windows"

                lift $ lift $ do
                    putStrLn "No Spigot server has found. Building..."
                    hFlush stdout

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
