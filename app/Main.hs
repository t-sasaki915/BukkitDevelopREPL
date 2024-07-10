module Main (main) where

import Constant
import ProcessIO (guaranteeSoftwareExistence)
import SpigotBuilder

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE, withExceptT)
import System.Directory
import System.FilePath ((</>))

data LauncherError = JavaNotFound String
                   | GitNotFound String
                   | MinecraftJarNotFound String
                   | SpigotBuildFailure SpigotBuildError

instance Show LauncherError where
    show (JavaNotFound s) =
        "The launcher could not find a Java executable: " ++ s
    show (GitNotFound s) =
        "The launcher could not find a Git executable: " ++ s
    show (MinecraftJarNotFound s) = unlines
        [ "The launcher could not find a Minecraft executable: " ++ s
        , "You need to launch Minecraft " ++ minecraftVersion ++ " at least once with the vanilla launcher."
        ]
    show (SpigotBuildFailure e) = show e

makeWorkingFolder :: ExceptT LauncherError IO ()
makeWorkingFolder = lift $
    createDirectoryIfMissing False workingDirPath

makeWorkingTempFolder :: ExceptT LauncherError IO ()
makeWorkingTempFolder = lift $
    createDirectoryIfMissing False workingTempDirPath

checkMinecraftJarExistence :: ExceptT LauncherError IO ()
checkMinecraftJarExistence = do
    homeDir <- lift getHomeDirectory
    let jarPath = minecraftVersionsDirPath homeDir </> minecraftVersion </> (minecraftVersion ++ ".jar")
    lift (doesFileExist jarPath) >>= \case
        True  -> return ()
        False -> throwE (MinecraftJarNotFound jarPath)

checkServerJarExistence :: ExceptT LauncherError IO Bool
checkServerJarExistence = lift $
    doesFileExist (workingDirPath </> spigotServerFileName)

program :: ExceptT LauncherError IO ()
program = do
    makeWorkingFolder
    withExceptT JavaNotFound (guaranteeSoftwareExistence "java.exe")
    withExceptT GitNotFound (guaranteeSoftwareExistence "git.exe")
    checkMinecraftJarExistence
    checkServerJarExistence >>= \case
        True  -> return ()
        False -> do
            lift $ putStrLn "There was no server executable. Building..."
            makeWorkingTempFolder
            withExceptT SpigotBuildFailure downloadBuildTools
            withExceptT SpigotBuildFailure buildSpigot
            withExceptT SpigotBuildFailure initialiseSpigotServer

main :: IO ()
main = runExceptT program >>= either print (const (return ()))
