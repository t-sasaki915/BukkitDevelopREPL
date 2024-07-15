module PluginInstaller (instalPlugins) where

import           AppOptions
import           FileIO
import           ProcessIO

import           Control.Monad                    (filterM, forM_, unless)
import           Control.Monad.Trans.Class        (lift)
import           Control.Monad.Trans.Except       (ExceptT, throwE)
import           Control.Monad.Trans.State.Strict (StateT, get)
import           Data.Functor                     ((<&>))
import           Data.Maybe                       (fromMaybe)
import           System.FilePath                  (takeFileName, (</>))
import           System.IO                        (hFlush, stdout)
import           Text.Regex.Posix                 ((=~))

instalPlugins :: ExceptT String (StateT AppOptions IO) ()
instalPlugins = do
    appOptions <- lift get
    let workDir = workingDir appOptions
        plugins = fromMaybe [] (pluginsToInstal appOptions)

    makeDirectory (workDir </> "plugins") "Failed to make plugins directory"

    pluginsFolderContents <- directoryContents (workDir </> "plugins")
        "Failed to enumerate the contents of plugins directory"
    installedJars <- filterM
        (`checkFileExistence` "Failed to enumerate the contents of plugins directory")
            pluginsFolderContents <&>
                filter (=~ ".+\\.jar$")

    forM_ installedJars $ \case
        installedJar | takeFileName installedJar `elem` map urlFileName plugins ->
            return ()

        unusedJar -> do
            deleteFile unusedJar ("Failed to delete an unused plugin '" ++ unusedJar ++ "'")
            lift $ lift $ do
                putStrLn ("Deleted an unused plugin '" ++ unusedJar ++ "'.")
                hFlush stdout

    let newLocalPlugins = flip filter plugins $ \case
            url | isUrl url -> False
            path            -> urlFileName path `notElem` map takeFileName installedJars

        newRemotePlugins = flip filter plugins $ \case
            url | isUrl url -> urlFileName url `notElem` map takeFileName installedJars
            _               -> False

    mapM_ instalLocalPlugin newLocalPlugins
    mapM_ instalRemotePlugin newRemotePlugins

instalLocalPlugin :: FilePath -> ExceptT String (StateT AppOptions IO) ()
instalLocalPlugin plugin = do
    workDir <- lift get <&> workingDir

    checkFileExistence plugin
        ("Failed to check the existence of a local plugin '" ++ plugin ++ "'") >>= \exists ->
            unless exists $ throwE ("Could not find a local plugin '" ++ plugin ++ "'")

    copyFileT plugin (workDir </> "plugins" </> takeFileName plugin)
        ("Failed to instal a local plugin '" ++ plugin ++ "'")

    lift $ lift $ do
        putStrLn ("Installed a local plugin '" ++ plugin ++ "'.")
        hFlush stdout

instalRemotePlugin :: FilePath -> ExceptT String (StateT AppOptions IO) ()
instalRemotePlugin plugin = do
    workDir <- lift get <&> workingDir

    let dlLocation = workDir </> "plugins" </> urlFileName plugin
    execProcess "curl.exe" ["-L", "-o", dlLocation, plugin] workDir
        ("Failed to execute curl.exe that was to download a remote plugin '" ++ plugin ++ "'") >>=
            expectExitSuccess
                ("Failed to download a remote plugin '" ++ plugin ++ "'")

    lift $ lift $ do
        putStrLn ("Installed a remote plugin '" ++ plugin ++ "'.")
        hFlush stdout

urlFileName :: FilePath -> String
urlFileName = \case
    url | isUrl url -> reverse (takeWhile (/= '/') (reverse url))
    path            -> takeFileName path

isUrl :: FilePath -> Bool
isUrl = (=~ "(http|ftp|https|file):\\/\\/.+$")
