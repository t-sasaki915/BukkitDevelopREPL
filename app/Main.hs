module Main (main) where

import AppOptions
import FileIO
import MinecraftClient
import ProcessIO
import SpigotServerSetup

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Control.Monad.Trans.State.Strict (StateT, runStateT, get)
import Options.Applicative
import System.Directory (getCurrentDirectory, getHomeDirectory)
import System.Exit (exitSuccess)
import System.Process (waitForProcess)

program :: ExceptT String (StateT AppOptions IO) ()
program = do
    appOptions <- lift get
    let workDir    = workingDir appOptions
        tmpWorkDir = tempWorkDir appOptions
        clientJar  = minecraftClientJarFile appOptions
        serverJar  = spigotServerJarFile appOptions

    makeDirectory workDir
    execProcess "java.exe" ["-version"] workDir >>= expectExitSuccess
    verifyFileExistence clientJar $
        "The launcher could not find Minecraft client executable: " ++ clientJar

    checkFileExistence serverJar >>= \case
        True  -> do
            clientProcess <- runMinecraftClient
            _ <- lift $ lift $ waitForProcess clientProcess -- temporary
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
                    (info (helper <*> appOptionsParser currentDir homeDir) idm)

    (result, _) <- runStateT (runExceptT program) appOptions
    either putStrLn (const (return ())) result
