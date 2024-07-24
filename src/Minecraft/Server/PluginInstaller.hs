module Minecraft.Server.PluginInstaller
    ( removeUnusedPlugins
    , installDynamicPlugins
    , installStaticPlugins
    ) where

import           Imports

import           AppState
import           CrossPlatform    (curlExecName)
import           ProcessIO

import           Network.Url      (FileExtension (Jar), isUrl,
                                   takeFileNameFromUrl)
import           System.Directory
import           System.FilePath  (takeFileName)

getFileName :: HasCallStack => FilePath -> String
getFileName url | isUrl url =
    takeFileNameFromUrl Jar url
getFileName path | path =~ (".+\\.jar$" :: String) =
    takeFileName path
getFileName path =
    error (printf "'%s' is not a path to a jar file." path)

getPluginsDir :: AppStateIO FilePath
getPluginsDir = getWorkingDir <&> (</> "plugins")

removeUnusedPlugins :: AppStateIO ()
removeUnusedPlugins = do
    pluginsDir     <- getPluginsDir
    dynamicPlugins <- getDynamicPlugins
    staticPlugins  <- getStaticPlugins

    unlessM (lift (doesDirectoryExist pluginsDir)) $
        lift (createDirectoryIfMissing True pluginsDir)

    dirContents <- map (pluginsDir </>) <$> lift (listDirectory pluginsDir)
    files <- filterM (lift . doesFileExist) dirContents

    let jarFiles           = filter (=~ (".+\\.jar$" :: String)) files
        dynamicPluginNames = map getFileName dynamicPlugins
        staticPluginNames  = map getFileName staticPlugins

    forM_ jarFiles $ \jarFile ->
        unless (takeFileName jarFile `elem` (dynamicPluginNames ++ staticPluginNames)) $ do
            lift (removeFile jarFile)

            putStrLn' (printf "Removed an unused plugin '%s'." (takeFileName jarFile))

installDynamicPlugins :: AppStateIO ()
installDynamicPlugins = do
    pluginsDir     <- getPluginsDir
    dynamicPlugins <- getDynamicPlugins

    forM_ dynamicPlugins $ \pluginUrl -> do
        let pluginName  = getFileName pluginUrl
            installPath = pluginsDir </> pluginName

        whenM (lift (doesFileExist installPath)) $ do
            lift (removeFile installPath)

            putStrLn' (printf "Removed an old plugin '%s'." pluginName)

        installPlugin pluginUrl

        putStrLn' (printf "Installed a plugin '%s'." pluginName)

installStaticPlugins :: AppStateIO ()
installStaticPlugins = do
    pluginsDir    <- getPluginsDir
    staticPlugins <- getStaticPlugins

    forM_ staticPlugins $ \pluginUrl -> do
        let pluginName  = getFileName pluginUrl
            installPath = pluginsDir </> pluginName

        lift (doesFileExist installPath) >>= \case
            True ->
                putStrLn' (printf "Skipped a plugin '%s'." pluginName)

            False -> do
                installPlugin pluginUrl

                putStrLn' (printf "Installed a plugin '%s'." pluginName)

installPlugin :: FilePath -> AppStateIO ()
installPlugin pluginUrl = do
    pluginsDir <- getPluginsDir

    let pluginName  = getFileName pluginUrl
        installPath = pluginsDir </> pluginName

    when (isUrl pluginUrl) $
        execProcess curlExecName ["-L", "-o", installPath, pluginUrl] pluginsDir >>=
            expectExitSuccess (printf "Failed to download a plugin '%s': %%s." pluginUrl)

    unless (isUrl pluginUrl) $
        lift (doesFileExist pluginUrl) >>= \case
            True ->
                lift (copyFile pluginUrl installPath)

            False ->
                error (printf "Could not find a plugin '%s'." pluginUrl)
