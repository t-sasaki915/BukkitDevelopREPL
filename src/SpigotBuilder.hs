module SpigotBuilder (SpigotBuildError(..), downloadBuildTools, buildSpigot) where

import Constant

import Control.Exception (try)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, throwE)
import GHC.IO.Exception (ExitCode (ExitSuccess))
import System.Directory (copyFile)
import System.FilePath ((</>))
import System.IO.Error (ioeGetErrorString)
import System.Process (waitForProcess, runProcess)

data SpigotBuildError = BuildToolsDownloadError String
                      | BuildToolsError String

instance Show SpigotBuildError where
    show (BuildToolsDownloadError s) =
        "The launcher has failed to download BuildTools: " ++ s
    show (BuildToolsError s) =
        "BuildTools has failed to build a Minecraft server: " ++ s

downloadBuildTools :: ExceptT SpigotBuildError IO ()
downloadBuildTools = do
    let args = ["-L", "-o", workingTempDirPath </> "BuildTools.jar", buildToolsUrl]
    process <- lift $ try (runProcess "curl.exe" args Nothing Nothing Nothing Nothing Nothing)
    case process of
        Right handler ->
            lift (waitForProcess handler) >>= \case
                ExitSuccess -> return ()
                exitCode    -> throwE (BuildToolsDownloadError (show exitCode))

        Left e ->
            throwE (BuildToolsDownloadError (ioeGetErrorString e))

buildSpigot :: ExceptT SpigotBuildError IO ()
buildSpigot = do
    let args = ["-jar", "BuildTools.jar", "--rev", minecraftVersion]
    process <- lift $ try (runProcess "java.exe" args (Just workingTempDirPath) Nothing Nothing Nothing Nothing)
    case process of
        Right handler ->
            lift (waitForProcess handler) >>= \case
                ExitSuccess -> lift $ copyFile (workingTempDirPath </> spigotServerFileName) (workingDirPath </> spigotServerFileName)
                exitCode    -> throwE (BuildToolsError (show exitCode))

        Left e ->
            throwE (BuildToolsError (ioeGetErrorString e))
