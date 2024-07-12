module PluginInstaller (instalPlugins) where

import AppOptions
import FileIO

import Control.Monad (when, unless)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, throwE)
import Control.Monad.Trans.State.Strict (StateT, get)
import Data.Functor ((<&>))
import System.FilePath ((</>), takeFileName)

instalPlugins :: ExceptT String (StateT AppOptions IO) ()
instalPlugins = do
    appOptions <- lift get
    let workDir = workingDir appOptions

    case pluginsToInstal appOptions of
        Just plugins -> do
            checkDirectoryExistence (workDir </> "plugins")
                "Failed to check the existence of plugins directory" >>= \exists ->
                    when exists $ deleteDirectory (workDir </> "plugins")
                        "Failed to delete plugins directory"
            makeDirectory (workDir </> "plugins")
                "Failed to make plugins directory"

            mapM_ instalPlugin plugins

        Nothing ->
            return ()

instalPlugin :: FilePath -> ExceptT String (StateT AppOptions IO) ()
instalPlugin plugin = do
    workDir <- lift get <&> workingDir

    checkFileExistence plugin
        ("Failed to check the existence of a plugin '" ++ plugin ++ "'") >>= \exists ->
            unless exists $ throwE ("Failed to read a plugin '" ++ plugin ++ "'")

    copyFileT plugin (workDir </> "plugins" </> takeFileName plugin)
        ("Failed to instal a plugin '" ++ plugin ++ "'")

    lift $ lift $ putStrLn ("Installed a plugin: " ++ plugin)
