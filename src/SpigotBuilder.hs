module SpigotBuilder
    ( SpigotBuildError(..)
    , downloadBuildTools
    , buildSpigot
    , initialiseSpigotServer
    ) where

import Constant
import ProcessIO (executeProcess, executeJava)

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, withExceptT)
import System.Directory (copyFile)
import System.FilePath ((</>))

data SpigotBuildError = BuildToolsDownloadError String
                      | BuildToolsError String
                      | SpigotInitialisationError String

instance Show SpigotBuildError where
    show (BuildToolsDownloadError s) =
        "The launcher has failed to download BuildTools: " ++ s
    show (BuildToolsError s) =
        "BuildTools has failed to build a Minecraft server: " ++ s
    show (SpigotInitialisationError s) =
        "The launcher has failed to initialise a Minecraft server: " ++ s

downloadBuildTools :: ExceptT SpigotBuildError IO ()
downloadBuildTools =
    withExceptT BuildToolsDownloadError $
        executeProcess "curl.exe"
            ["-L", "-o", workingTempDirPath </> "BuildTools.jar", buildToolsUrl]

buildSpigot :: ExceptT SpigotBuildError IO ()
buildSpigot = do
    withExceptT BuildToolsError $
        executeJava ["-jar", "BuildTools.jar", "--rev", minecraftVersion] workingTempDirPath

    lift $ copyFile (workingTempDirPath </> spigotServerFileName) (workingDirPath </> spigotServerFileName)

initialiseSpigotServer :: ExceptT SpigotBuildError IO ()
initialiseSpigotServer = do
    withExceptT SpigotInitialisationError $
        executeJava ["-jar", spigotServerFileName, "nogui"] workingTempDirPath

    properties <- lift $ readFile (workingTempDirPath </> "server.properties")
    let modifiedProperties = foldl
            (\lst -> \case
                "online-mode=true" ->
                    lst ++ ["online-mode=false"]
                
                "gamemode=survival" ->
                    lst ++ ["gamemode=creative"]

                other ->
                    lst ++ [other]
            ) [] (lines properties)

    lift $ writeFile (workingDirPath </> "server.properties") (unlines modifiedProperties)

    withExceptT SpigotInitialisationError $
        executeJava ["-jar", spigotServerFileName, "nogui"] workingDirPath
