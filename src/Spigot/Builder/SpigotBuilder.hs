module Spigot.Builder.SpigotBuilder () where

import           AppState
import           CLIOptions.CLIOptions            (CLIOptions (..))
import           ProcessIO
import           Spigot.Builder.Resource          (buildToolsUrl)

import           Control.Monad.Trans.Class        (lift)
import           Control.Monad.Trans.State.Strict (get)
import           Data.Functor                     ((<&>))
import           System.FilePath                  ((</>))

downloadBuildTools :: AppStateIO ()
downloadBuildTools = do
    cliOpts <- lift get <&> cliOptions
    let buildDir = workingDir cliOpts </> "build"
        downloadPath = buildDir </> "BuildTools.jar"

    execProcess "curl.exe" ["-L", "-o", downloadPath, buildToolsUrl] buildDir
        "Failed to execute curl.exe that was to download BuildTools" >>=
            expectExitSuccess
                "Failed to download BuildTools"

