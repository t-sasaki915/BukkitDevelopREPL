{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module AppState where

import           CLIOptions.CLIOptions            (CLIOptions (..))
import           CLIOptions.Parser                (parseCLIOptions)
import           Config.Config
import           Config.Loader                    (loadConfig)

import           Control.Monad.Trans.Class        (lift)
import           Control.Monad.Trans.Except       (ExceptT)
import           Control.Monad.Trans.State.Strict (StateT, get)
import           Data.Functor                     ((<&>))
import           System.FilePath                  ((</>))

type AppStateIO = ExceptT String (StateT AppState IO)

data AppState = AppState
    { cliOptions :: CLIOptions
    , config     :: Config
    }
    deriving Show

initialState :: IO AppState
initialState = do
    cliOpts <- parseCLIOptions
    conf    <- loadConfig (configFile cliOpts)

    let constructor = AppState
    return (constructor cliOpts conf)

getWorkingDir :: AppStateIO FilePath
getWorkingDir = lift get <&> (workingDir . applicationConfig . config)

getClientWorkingDir :: AppStateIO FilePath
getClientWorkingDir = getWorkingDir <&> (</> "client")

getBuildDir :: AppStateIO FilePath
getBuildDir = getWorkingDir <&> (</> "build")

getBuildToolsPath :: AppStateIO FilePath
getBuildToolsPath = getBuildDir <&> (</> "BuildTools.jar")

getMinecraftDir :: AppStateIO FilePath
getMinecraftDir = lift get <&> (minecraftDir . cliOptions)

getMinecraftAssetsDir :: AppStateIO FilePath
getMinecraftAssetsDir = getMinecraftDir <&> (</> "assets")

getMinecraftLibrariesDir :: AppStateIO FilePath
getMinecraftLibrariesDir = getMinecraftDir <&> (</> "libraries")

getMinecraftVersionsDir :: AppStateIO FilePath
getMinecraftVersionsDir = getMinecraftDir <&> (</> "versions")
