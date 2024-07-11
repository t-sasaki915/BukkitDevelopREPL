module PluginInstaller (instalPlugins) where

import AppOptions
import FileIO

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.State.Strict (StateT, get)
import Data.Functor ((<&>))
import System.FilePath ((</>), takeFileName)

instalPlugins :: ExceptT String (StateT AppOptions IO) ()
instalPlugins = do
    appOptions <- lift get
    let workDir = workingDir appOptions

    case pluginsToInstal appOptions of
        Just plugins -> do
            checkDirectoryExistence (workDir </> "plugins") >>= \case
                True  -> deleteDirectory (workDir </> "plugins")
                False -> return ()
            makeDirectory (workDir </> "plugins")

            mapM_ instalPlugin plugins

        Nothing ->
            return ()

instalPlugin :: FilePath -> ExceptT String (StateT AppOptions IO) ()
instalPlugin plugin = do
    workDir <- lift get <&> workingDir

    verifyFileExistence plugin ("Failed to read a plugin: " ++ plugin)

    copyFileT plugin (workDir </> "plugins" </> takeFileName plugin)

    lift $ lift $ putStrLn ("Installed a plugin: " ++ plugin)
