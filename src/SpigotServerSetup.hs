module SpigotServerSetup
    ( downloadBuildTools
    , buildSpigot
    , setupSpigotServer
    ) where

import AppOptions
import Constant (buildToolsUrl, minecraftVersion)
import FileIO
import ProcessIO

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.State.Strict (StateT, get)
import Data.Functor ((<&>))
import System.FilePath ((</>), takeFileName)

downloadBuildTools :: ExceptT String (StateT AppOptions IO) ()
downloadBuildTools = do
    dlLocation <- lift (get <&> buildToolsJarFile)
    tmpWorkDir <- lift (get <&> tempWorkDir)
    execProcess "curl.exe" ["-L", "-o", dlLocation, buildToolsUrl] tmpWorkDir >>=
        expectExitSuccess

buildSpigot :: ExceptT String (StateT AppOptions IO) ()
buildSpigot = do
    appOptions <- lift get
    let tmpWorkDir    = tempWorkDir appOptions
        buildToolsJar = buildToolsJarFile appOptions
        serverJar     = spigotServerJarFile appOptions
        tmpServerJar  = tmpWorkDir </> takeFileName serverJar

    execProcess "java.exe" ["-jar", buildToolsJar, "--rev", minecraftVersion] tmpWorkDir >>=
        expectExitSuccess >>
            copyFileT tmpServerJar serverJar

setupSpigotServer :: ExceptT String (StateT AppOptions IO) ()
setupSpigotServer = do
    appOptions <- lift get
    let workDir      = workingDir appOptions
        tmpWorkDir   = tempWorkDir appOptions
        serverJar    = spigotServerJarFile appOptions
        tmpServerJar = tmpWorkDir </> takeFileName serverJar

    execProcess "java.exe" ["-jar", tmpServerJar] tmpWorkDir >>=
        expectExitSuccess
    
    serverProperties <- readFileT (tmpWorkDir </> "server.properties")
    let customisedProperties = customiseServerProperties serverProperties
    writeFileT (workDir </> "server.properties") customisedProperties

    execProcess "java.exe" ["-jar", serverJar] workDir >>=
        expectExitSuccess


customiseServerProperties :: String -> String
customiseServerProperties properties = unlines $ foldl (\lst -> \case
        "online-mode=true"           -> lst ++ ["online-mode=false"]
        "gamemode=survival"          -> lst ++ ["gamemode=creative"]
        "motd=A Minecraft Server"    -> lst ++ ["motd=Plugin DEV Server"]
        "max-players=20"             -> lst ++ ["max-players=3"]
        "enable-command-block=false" -> lst ++ ["enable-command-block=true"]
        other                        -> lst ++ [other]
    ) [] (lines properties)
