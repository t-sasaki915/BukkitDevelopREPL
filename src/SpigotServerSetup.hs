module SpigotServerSetup
    ( downloadBuildTools
    , buildSpigot
    , setupSpigotServer
    ) where

import           AppOptions
import           Constant

import           FileIO
import           ProcessIO

import           Control.Monad.Trans.Class        (lift)
import           Control.Monad.Trans.Except       (ExceptT)
import           Control.Monad.Trans.State.Strict (StateT, get)
import           Data.Functor                     ((<&>))
import           System.FilePath                  (takeFileName, (</>))
import           System.IO                        (hFlush, stdout)

downloadBuildTools :: ExceptT String (StateT AppOptions IO) ()
downloadBuildTools = do
    dlLocation <- lift (get <&> buildToolsJarFile)
    tmpWorkDir <- lift (get <&> tempWorkDir)
    execProcess "curl.exe" ["-L", "-o", dlLocation, buildToolsUrl] tmpWorkDir
        "Failed to execute curl.exe that was to download BuildTools" >>=
            expectExitSuccess
                "Failed to download BuildTools"

buildSpigot :: ExceptT String (StateT AppOptions IO) ()
buildSpigot = do
    appOptions <- lift get
    let tmpWorkDir    = tempWorkDir appOptions
        buildToolsJar = buildToolsJarFile appOptions
        serverJar     = spigotServerJarFile appOptions
        tmpServerJar  = tmpWorkDir </> takeFileName serverJar

    execProcess "java.exe" ["-jar", buildToolsJar, "--rev", minecraftVersion] tmpWorkDir
        "Failed to execute java.exe that was to build a Spigot server" >>=
            expectExitSuccess
                "Failed to build a Spigot server" >>
                    copyFileT tmpServerJar serverJar
                        "Failed to copy a Spigot server executable"

setupSpigotServer :: ExceptT String (StateT AppOptions IO) ()
setupSpigotServer = do
    appOptions <- lift get
    let workDir      = workingDir appOptions
        tmpWorkDir   = tempWorkDir appOptions
        serverJar    = spigotServerJarFile appOptions
        tmpServerJar = tmpWorkDir </> takeFileName serverJar

    execProcess "java.exe" ["-jar", tmpServerJar] tmpWorkDir
        "Failed to execute java.exe that was to generate a template of server.properties" >>=
            expectExitSuccess
                "Failed to generate a template of server.properties"

    serverProperties <- readFileT (tmpWorkDir </> "server.properties")
        "Failed to read a template of server.properties"
    let customisedProperties = customiseServerProperties serverProperties
    writeFileT (workDir </> "server.properties") customisedProperties
        "Failed to generate a customised server.properties"

    execProcess "java.exe" ["-jar", serverJar] workDir
        "Failed to execute java.exe that was to do the first server startup" >>=
            expectExitSuccess
                "Failed to do the first server startup"

    lift $ lift $ do
        putStrLn ("A Spigot server has successfully built and stored in " ++ workDir ++ ".")
        putStrLn ("Please edit " ++ (workDir </> "eula.txt") ++ " to accept the eula.")
        hFlush stdout


customiseServerProperties :: String -> String
customiseServerProperties properties = unlines $ map (\case
        "online-mode=true"           -> "online-mode=false"
        "gamemode=survival"          -> "gamemode=creative"
        "motd=A Minecraft Server"    -> "motd=Plugin DEV Server"
        "max-players=20"             -> "max-players=3"
        "enable-command-block=false" -> "enable-command-block=true"
        other                        -> other
    ) (lines properties)
