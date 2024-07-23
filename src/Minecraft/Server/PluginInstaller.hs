module Minecraft.Server.PluginInstaller
    ( initialisePluginFileNameMap
    , removeUnusedPlugins
    , installDynamicPlugins
    , installStaticPlugins
    ) where

import           AppState
import           CrossPlatform              (curlExecName)
import           FileIO
import           ProcessIO

import           Control.Monad              (filterM, forM, forM_)
import           Control.Monad.Extra        (unlessM)
import           Control.Monad.Trans.Except (throwE)
import           Data.Functor               ((<&>))
import           Network.Url
import           System.FilePath            (takeFileName, (</>))
import           Text.Regex.Posix           ((=~))

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
                "Failed to remove an unused plugin '" ++ takeFileName jarFile ++ "'"

            putStrLn' ("Removed an unused plugin '" ++ takeFileName jarFile ++ "'.")

        _ ->
            return ()

installDynamicPlugins :: AppStateIO ()
installDynamicPlugins = do
    pluginsDir     <- getWorkingDir <&> (</> "plugins")
    dynamicPlugins <- getDynamicPlugins

    forM_ dynamicPlugins $ \plugin -> do
        pluginName <- getDynamicPluginFileName plugin

        checkFileExistence (pluginsDir </> pluginName)
            ("Failed to check the existence of '" ++ (pluginsDir </> pluginName) ++ "'") >>= \case
                True -> do
                    removeFile' (pluginsDir </> pluginName) $
                        "Failed to remove an old plugin '" ++ (pluginsDir </> pluginName) ++ "'"

                    putStrLn' ("Removed an old plugin '" ++ pluginName ++ "'.")

                False ->
                    return ()

    forM_ dynamicPlugins $ \case
        pluginPath | not (isUrl pluginPath) ->
            checkFileExistence pluginPath
                ("Failed to check the existence of '" ++ pluginPath ++ "'") >>= \case
                    True -> do
                        copyFile' pluginPath (pluginsDir </> takeFileName pluginPath) $
                            "Failed to copy a plugin '" ++ pluginPath ++ "'"

                        putStrLn' ("Installed a plugin '" ++ takeFileName pluginPath ++ "'.")

                    False ->
                        throwE ("Could not find a plugin '" ++ pluginPath ++ "'.")

        pluginUrl -> do
            pluginName <- getDynamicPluginFileName pluginUrl
            let downloadPath = pluginsDir </> pluginName

            execProcess curlExecName ["-L", "-o", downloadPath, pluginUrl] pluginsDir
                ("Failed to execute curl that was to download a plugin '" ++ pluginUrl ++ "'") >>=
                    expectExitSuccess
                        ("Failed to download a plugin '" ++ pluginUrl ++ "'")

            putStrLn' ("Installed a plugin '" ++ pluginName ++ "'.")

installStaticPlugins :: AppStateIO ()
installStaticPlugins = do
    pluginsDir    <- getWorkingDir <&> (</> "plugins")
    staticPlugins <- getStaticPlugins

    forM_ staticPlugins $ \plugin -> do
        pluginName <- getStaticPluginFileName plugin

        checkFileExistence (pluginsDir </> pluginName)
            ("Failed to check the existence of '" ++ (pluginsDir </> pluginName) ++ "'") >>= \case
                True ->
                    putStrLn' ("Skipped a plugin '" ++ pluginName ++ "'.")

                False | not (isUrl plugin) ->
                    checkFileExistence plugin
                        ("Failed to check the existence of '" ++ plugin ++ "'") >>= \case
                            True -> do
                                copyFile' plugin (pluginsDir </> pluginName) $
                                    "Failed to copy a plugin '" ++ plugin ++ "'"

                                putStrLn' ("Installed a plugin '" ++ pluginName ++ "'.")

                            False ->
                                throwE ("Could not find a plugin '" ++ plugin ++ "'.")

                False -> do
                    let downloadPath = pluginsDir </> pluginName

                    execProcess curlExecName ["-L", "-o", downloadPath, plugin] pluginsDir
                        ("Failed to execute curl that was to download a plugin '" ++ plugin ++ "'") >>=
                            expectExitSuccess
                                ("Failed to download a plugin '" ++ plugin ++ "'")

                    putStrLn' ("Installed a plugin '" ++ pluginName ++ "'.")
