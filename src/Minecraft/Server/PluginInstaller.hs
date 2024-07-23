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
import           Text.Printf                (printf)
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
        (printf "Failed to check the existence of a directory '%s': %%s." pluginsDir) >>= \case
            True  -> return ()
            False ->
                makeDirectory pluginsDir $
                    printf "Failed to create a directory '%s': %%s." pluginsDir

    dirContents <- directoryContents pluginsDir $
        printf "Failed to enumerate the contents of '%s': %%s." pluginsDir
    files <- flip filterM dirContents $ \content ->
        checkFileExistence content $
            printf "Failed to check the existence of '%s': %%s." content

    let jarFiles = filter (=~ (".+\\.jar$" :: String)) files

    dynamicPluginNames <- mapM getDynamicPluginFileName dynamicPlugins
    staticPluginNames  <- mapM getStaticPluginFileName staticPlugins

    forM_ jarFiles $ \case
        jarFile | takeFileName jarFile `notElem` (dynamicPluginNames ++ staticPluginNames) -> do
            removeFile' jarFile $
                printf "Failed to remove and unused plugin '%s': %%s." jarFile

            putStrLn' (printf "Removed an unused plugin '%s'." (takeFileName jarFile))

        _ ->
            return ()

installDynamicPlugins :: AppStateIO ()
installDynamicPlugins = do
    pluginsDir     <- getWorkingDir <&> (</> "plugins")
    dynamicPlugins <- getDynamicPlugins

    forM_ dynamicPlugins $ \plugin -> do
        pluginName <- getDynamicPluginFileName plugin

        checkFileExistence (pluginsDir </> pluginName)
            (printf "Failed to check the existence of '%s': %%s." (pluginsDir </> pluginName)) >>= \case
                True -> do
                    removeFile' (pluginsDir </> pluginName) $
                        printf "Failed to remove an old plugin '%s': %%s." (pluginsDir </> pluginName)

                    putStrLn' (printf "Removed an old plugin '%s'." pluginName)

                False ->
                    return ()

    forM_ dynamicPlugins $ \case
        pluginPath | not (isUrl pluginPath) ->
            checkFileExistence pluginPath
                (printf "Failed to check the existence of '%s': %%s." pluginPath) >>= \case
                    True -> do
                        copyFile' pluginPath (pluginsDir </> takeFileName pluginPath) $
                            printf "Failed to copy a plugin '%s' to '%s': %%s." pluginPath (pluginsDir </> takeFileName pluginPath)

                        putStrLn' (printf "Installed a plugin '%s'." (takeFileName pluginPath))

                    False ->
                        throwE (printf "Could not find a plugin '%s'." pluginPath)

        pluginUrl -> do
            pluginName <- getDynamicPluginFileName pluginUrl
            let downloadPath = pluginsDir </> pluginName

            execProcess curlExecName ["-L", "-o", downloadPath, pluginUrl] pluginsDir
                (printf "Failed to execute curl that was to download a plugin '%s': %%s." pluginUrl) >>=
                    expectExitSuccess
                        (printf "Failed to download a plugin '%s': %%s." pluginUrl)

            putStrLn' (printf "Installed a plugin '%s'." pluginName)

installStaticPlugins :: AppStateIO ()
installStaticPlugins = do
    pluginsDir    <- getWorkingDir <&> (</> "plugins")
    staticPlugins <- getStaticPlugins

    forM_ staticPlugins $ \plugin -> do
        pluginName <- getStaticPluginFileName plugin

        checkFileExistence (pluginsDir </> pluginName)
            (printf "Failed to check the existence of '%s': %%s." (pluginsDir </> pluginName)) >>= \case
                True ->
                    putStrLn' (printf "Skipped a plugin '%s'." pluginName)

                False | not (isUrl plugin) ->
                    checkFileExistence plugin
                        (printf "Failed to check the existence of '%s': %%s." plugin) >>= \case
                            True -> do
                                copyFile' plugin (pluginsDir </> pluginName) $
                                    printf "Failed to copy a plugin '%s' to '%s': %%s." plugin (pluginsDir </> pluginName)

                                putStrLn' (printf "Installed a plugin '%s'." pluginName)

                            False ->
                                throwE (printf "Could not find a plugin '%s'." plugin)

                False -> do
                    let downloadPath = pluginsDir </> pluginName

                    execProcess curlExecName ["-L", "-o", downloadPath, plugin] pluginsDir
                        (printf "Failed to execute curl that was to download a plugin '%s': %%s." plugin) >>=
                            expectExitSuccess
                                (printf "Failed to download a plugin '%s': %%s." plugin)

                    putStrLn' (printf "Installed a plugin '%s'." pluginName)
