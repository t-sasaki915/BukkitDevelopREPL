module Main (main) where

import Control.Exception (try)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import GHC.IO.Exception (ExitCode (ExitSuccess))
import System.Directory
import System.FilePath ((</>))
import System.IO.Error (ioeGetErrorString)
import System.Process (waitForProcess, runProcess)

data LauncherError = JavaNotFound String
                   | MinecraftJarNotFound String

instance Show LauncherError where
    show (JavaNotFound s) =
        "The launcher could not find a Java executable: " ++ s
    show (MinecraftJarNotFound s) = unlines
        [ "The launcher could not find a Minecraft executable: " ++ s
        , "You need to launch Minecraft " ++ minecraftVersion ++ " at least once with the vanilla launcher."
        ]

minecraftVersion :: String
minecraftVersion = "1.20.1"

makeWorkingFolder :: ExceptT LauncherError IO ()
makeWorkingFolder = do
    _ <- lift $ createDirectoryIfMissing False ("." </> "run")
    return ()

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

checkMinecraftJarExistence :: ExceptT LauncherError IO ()
checkMinecraftJarExistence = do
    homeDir <- lift getHomeDirectory
    let jarPath = homeDir </> "AppData" </> "Roaming" </> ".minecraft" </> "versions" </> minecraftVersion </> (minecraftVersion ++ ".jar")
    lift (doesFileExist jarPath) >>= \case
        True  -> return ()
        False -> throwE (MinecraftJarNotFound jarPath)

program :: ExceptT LauncherError IO ()
program = do
    makeWorkingFolder
    checkJavaExistence
    checkMinecraftJarExistence

main :: IO ()
main = runExceptT program >>= either print (const (return ()))
