module Main (main) where

import Constant
import SpigotBuilder (SpigotBuildError(..), downloadBuildTools, buildSpigot)

import Control.Exception (try)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE, withExceptT)
import GHC.IO.Exception (ExitCode (ExitSuccess))
import System.Directory
import System.FilePath ((</>))
import System.IO.Error (ioeGetErrorString)
import System.Process (waitForProcess, runProcess)

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

checkJavaExistence :: ExceptT LauncherError IO ()
checkJavaExistence = do
    process <- lift $ try (runProcess "java.exe" ["-version"] Nothing Nothing Nothing Nothing Nothing)
    case process of
        Right handler ->
            lift (waitForProcess handler) >>= \case
                ExitSuccess -> return ()
                exitCode    -> throwE (JavaNotFound (show exitCode))

        Left e ->
            throwE (JavaNotFound (ioeGetErrorString e))

checkGitExistence :: ExceptT LauncherError IO ()
checkGitExistence = do
    process <- lift $ try (runProcess "git.exe" ["--version"] Nothing Nothing Nothing Nothing Nothing)
    case process of
        Right handler ->
            lift (waitForProcess handler) >>= \case
                ExitSuccess -> return ()
                exitCode    -> throwE (GitNotFound (show exitCode))
        
        Left e ->
            throwE (GitNotFound (ioeGetErrorString e))

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
    checkJavaExistence
    checkGitExistence
    checkMinecraftJarExistence
    checkServerJarExistence >>= \case
        True  -> return ()
        False -> do
            lift $ putStrLn "There was no server executable. Building..."
            makeWorkingTempFolder
            withExceptT SpigotBuildFailure downloadBuildTools
            withExceptT SpigotBuildFailure buildSpigot

main :: IO ()
main = runExceptT program >>= either print (const (return ()))
