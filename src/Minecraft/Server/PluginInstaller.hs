module Minecraft.Server.PluginInstaller
    ( initialisePluginFileNameMap
    , removeUnusedPlugins
    , installDynamicPlugins
    , installStaticPlugins
    ) where

import           Imports

import           AppState
import           CrossPlatform    (curlExecName)
import           ProcessIO

import           Network.Url
import           System.Directory
import           System.FilePath  (takeFileName)

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
        getFileName filePath | isUrl filePath = lift $ getFileNameFromUrl Jar filePath
        getFileName filePath = return (takeFileName filePath)

removeUnusedPlugins :: AppStateIO ()
removeUnusedPlugins = do
    pluginsDir     <- getWorkingDir <&> (</> "plugins")
    dynamicPlugins <- getDynamicPlugins
    staticPlugins  <- getStaticPlugins

    unlessM (lift (doesDirectoryExist pluginsDir)) $
        lift (createDirectoryIfMissing True pluginsDir)

    dirContents <- map (pluginsDir </>) <$> lift (listDirectory pluginsDir)
    files <- filterM (lift . doesFileExist) dirContents

    let jarFiles = filter (=~ (".+\\.jar$" :: String)) files

    dynamicPluginNames <- mapM getDynamicPluginFileName dynamicPlugins
    staticPluginNames  <- mapM getStaticPluginFileName staticPlugins

    forM_ jarFiles $ \jarFile ->
        unless (takeFileName jarFile `elem` (dynamicPluginNames ++ staticPluginNames)) $ do
            lift (removeFile jarFile)

            putStrLn' (printf "Removed an unused plugin '%s'." (takeFileName jarFile))

installDynamicPlugins :: AppStateIO ()
installDynamicPlugins = do
    pluginsDir     <- getWorkingDir <&> (</> "plugins")
    dynamicPlugins <- getDynamicPlugins

    forM_ dynamicPlugins $ \plugin -> do
        pluginName <- getDynamicPluginFileName plugin
        let pluginPath = pluginsDir </> pluginName

        whenM (lift (doesFileExist pluginPath)) $ do
            lift (removeFile pluginPath)

            putStrLn' (printf "Removed an old plugin '%s'." pluginName)

    forM_ dynamicPlugins $ \case
        pluginPath | not (isUrl pluginPath) ->
            lift (doesFileExist pluginPath) >>= \case
                    True -> do
                        lift (copyFile pluginPath (pluginsDir </> takeFileName pluginPath))

                        putStrLn' (printf "Installed a plugin '%s'." (takeFileName pluginPath))

                    False ->
                        error (printf "Could not find a plugin '%s'." pluginPath)

        pluginUrl -> do
            pluginName <- getDynamicPluginFileName pluginUrl
            let downloadPath = pluginsDir </> pluginName

            execProcess curlExecName ["-L", "-o", downloadPath, pluginUrl] pluginsDir >>=
                expectExitSuccess (printf "Failed to download a plugin '%s': %%s." pluginUrl)

            putStrLn' (printf "Installed a plugin '%s'." pluginName)

installStaticPlugins :: AppStateIO ()
installStaticPlugins = do
    pluginsDir    <- getWorkingDir <&> (</> "plugins")
    staticPlugins <- getStaticPlugins

    forM_ staticPlugins $ \plugin -> do
        pluginName <- getStaticPluginFileName plugin

        lift (doesFileExist (pluginsDir </> pluginName)) >>= \case
            True ->
                putStrLn' (printf "Skipped a plugin '%s'." pluginName)

            False | not (isUrl plugin) ->
                lift (doesFileExist plugin) >>= \case
                        True -> do
                            lift (copyFile plugin (pluginsDir </> pluginName))

                            putStrLn' (printf "Installed a plugin '%s'." pluginName)

                        False ->
                            error (printf "Could not find a plugin '%s'." plugin)

            False -> do
                let downloadPath = pluginsDir </> pluginName

                execProcess curlExecName ["-L", "-o", downloadPath, plugin] pluginsDir >>=
                    expectExitSuccess (printf "Failed to download a plugin '%s': %%s." plugin)

                putStrLn' (printf "Installed a plugin '%s'." pluginName)
