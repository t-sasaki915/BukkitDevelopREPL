module Minecraft.Server.PluginInstaller
    ( initialisePluginFileNameMap
    , removeUnusedPlugins
    , installDynamicPlugins
    , installStaticPlugins
    ) where

import           AppState
import           FileIO

import           Control.Monad       (filterM, forM, forM_)
import           Control.Monad.Extra (unlessM)
import           Data.Functor        ((<&>))
import           Network.Url
import           System.FilePath     (takeFileName, (</>))
import           Text.Regex.Posix    ((=~))

initialisePluginFileNameMap :: AppStateIO ()
initialisePluginFileNameMap = do
    unlessM isDynamicPluginFileNameMapInitialised $ do
        dynamicPlugins <- getDynamicPlugins
        dynamicPluginNames <- forM dynamicPlugins $ \plugin -> do
            pluginName <- getFileName plugin
            return (plugin, pluginName)

        setDynamicPluginFileNameMap dynamicPluginNames

    unlessM isStaticPluginFileNameMapInitialised $ do
        staticPlugins <- getStaticPlugins
        staticPluginNames <- forM staticPlugins $ \plugin -> do
            pluginName <- getFileName plugin
            return (plugin, pluginName)

        setStaticPluginFileNameMap staticPluginNames
    where
        getFileName :: FilePath -> AppStateIO String
        getFileName filePath | isUrl filePath = getFileNameFromUrl Jar filePath
        getFileName filePath = return (takeFileName filePath)

removeUnusedPlugins :: AppStateIO ()
removeUnusedPlugins = do
    pluginsDir     <- getWorkingDir <&> (</> "plugins")
    dynamicPlugins <- getDynamicPlugins
    staticPlugins  <- getStaticPlugins

    checkDirectoryExistence pluginsDir
        ("Failed to check the existence of a directory '" ++ pluginsDir ++ "'") >>= \case
            True  -> return ()
            False ->
                makeDirectory pluginsDir $
                    "Failed to create a directory '" ++ pluginsDir ++ "'"

    dirContents <- directoryContents pluginsDir $
        "Failed to enumerate the contents of '" ++ pluginsDir ++ "'"
    files <- flip filterM dirContents $ \content ->
        checkFileExistence content $
            "Failed to check the existence of '" ++ content ++ "'"

    let jarFiles = filter (=~ (".+\\.jar$" :: String)) files

    dynamicPluginNames <- mapM getDynamicPluginFileName dynamicPlugins
    staticPluginNames  <- mapM getStaticPluginFileName staticPlugins

    forM_ jarFiles $ \case
        jarFile | takeFileName jarFile `notElem` (dynamicPluginNames ++ staticPluginNames) -> do
            removeFile' jarFile $
                "Failed to remove an unused plugin '" ++ jarFile ++ "'"

            putStrLn' ("Removed an unused plugin '" ++ jarFile ++ "'.")

        _ ->
            return ()

installDynamicPlugins :: AppStateIO ()
installDynamicPlugins = return ()

installStaticPlugins :: AppStateIO ()
installStaticPlugins = return ()
