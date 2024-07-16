module Spigot.Builder.SpigotBuilder () where

import           AppState
import           ProcessIO
import           Spigot.Builder.Resource          (buildToolsUrl)

downloadBuildTools :: AppStateIO ()
downloadBuildTools = do
    downloadPath <- getBuildToolsPath
    buildDir     <- getBuildDir

    execProcess "curl.exe" ["-L", "-o", downloadPath, buildToolsUrl] buildDir
        "Failed to execute curl.exe that was to download BuildTools" >>=
            expectExitSuccess
                "Failed to download BuildTools"
