{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module AppState where

import           CLIOptions.CLIOptions            (CLIOptions (..))
import           CLIOptions.Parser                (parseCLIOptions)
import           Config.Config
import           Config.Loader                    (loadConfig)
import           Minecraft.MinecraftVersion       (MinecraftVersion)

import           Control.Monad.Trans.Class        (lift)
import           Control.Monad.Trans.Except       (ExceptT)
import           Control.Monad.Trans.State.Strict (StateT, get, put)
import           Data.Functor                     ((<&>))
import           System.Directory                 (makeAbsolute)
import           System.FilePath                  ((</>))
import           System.IO                        (hFlush, stdout)
import           System.Process                   (ProcessHandle)

type AppStateIO = ExceptT String (StateT AppState IO)

data AppState = AppState
    { clients_    :: [(String, ProcessHandle)]
    , cliOptions_ :: CLIOptions
    , config_     :: Config
    }

initialState :: IO AppState
initialState = do
    cliOpts <- parseCLIOptions
    conf    <- loadConfig (configFile cliOpts)

    let constructor = AppState []
    return (constructor cliOpts conf)

absolutePath :: FilePath -> AppStateIO FilePath
absolutePath = lift . lift . makeAbsolute

putStrLn' :: String -> AppStateIO ()
putStrLn' msg = lift $ lift $ do
    putStrLn msg
    hFlush stdout

getClients :: AppStateIO [(String, ProcessHandle)]
getClients = lift get <&> clients_

getWorkingDir :: AppStateIO FilePath
getWorkingDir = lift get >>=
    absolutePath . workingDir . applicationConfig . config_

getClientWorkingDir :: AppStateIO FilePath
getClientWorkingDir = getWorkingDir <&> (</> "client")

getBuildDir :: AppStateIO FilePath
getBuildDir = getWorkingDir <&> (</> "build")

getBuildToolsPath :: AppStateIO FilePath
getBuildToolsPath = getBuildDir <&> (</> "BuildTools.jar")

getMinecraftDir :: AppStateIO FilePath
getMinecraftDir = lift get >>=
    absolutePath . minecraftDir . cliOptions_

getMinecraftAssetsDir :: AppStateIO FilePath
getMinecraftAssetsDir = getMinecraftDir <&> (</> "assets")

getMinecraftLibrariesDir :: AppStateIO FilePath
getMinecraftLibrariesDir = getMinecraftDir <&> (</> "libraries")

getMinecraftVersionsDir :: AppStateIO FilePath
getMinecraftVersionsDir = getMinecraftDir <&> (</> "versions")

getMinecraftBinDir :: AppStateIO FilePath
getMinecraftBinDir = getMinecraftDir <&> (</> "bin")

getClientDefaultVersion :: AppStateIO MinecraftVersion
getClientDefaultVersion = lift get <&> (clientDefaultVersion . clientConfig . config_)

getClientDefaultUsername :: AppStateIO String
getClientDefaultUsername = lift get <&> (clientDefaultUsername . clientConfig . config_)

getClientJvmOptions :: AppStateIO [String]
getClientJvmOptions = lift get <&> (clientJvmOptions . clientConfig . config_)

getServerVersion :: AppStateIO MinecraftVersion
getServerVersion = lift get <&> (serverVersion . serverConfig . config_)

getServerJarPath :: AppStateIO FilePath
getServerJarPath = getServerVersion >>= \ver ->
    getWorkingDir <&> (</> "spigot-" ++ show ver ++ ".jar")

getTemporaryServerJarPath :: AppStateIO FilePath
getTemporaryServerJarPath = getServerVersion >>= \ver ->
    getBuildDir <&> (</> "spigot-" ++ show ver ++ ".jar")

getServerJvmOptions :: AppStateIO [String]
getServerJvmOptions = lift get <&> (serverJvmOptions . serverConfig . config_)

registerNewClient :: String -> ProcessHandle -> AppStateIO ()
registerNewClient clientUsername clientHandle = do
    state <- lift get
    let updated = clients_ state ++ [(clientUsername, clientHandle)]

    lift $ put (AppState updated (cliOptions_ state) (config_ state))
