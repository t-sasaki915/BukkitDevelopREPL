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
import           System.Process                   (ProcessHandle,
                                                   getProcessExitCode)

type AppStateIO = ExceptT String (StateT AppState IO)

data AppState = AppState
    { clients_    :: [(ClientInfo, ProcessHandle)]
    , cliOptions_ :: CLIOptions
    , config_     :: Config
    }

data ClientInfo = ClientInfo
    { runningClientName    :: String
    , runningClientVersion :: MinecraftVersion
    }
    deriving Eq

initialState :: IO AppState
initialState = do
    cliOpts <- parseCLIOptions
    conf    <- loadConfig (configFile cliOpts)

    let constructor = AppState []
    return (constructor cliOpts conf)

getAutoexecCommands :: AppState -> [String]
getAutoexecCommands = autoexecCommands . applicationConfig . config_

absolutePath :: FilePath -> AppStateIO FilePath
absolutePath = lift . lift . makeAbsolute

putStrLn' :: String -> AppStateIO ()
putStrLn' msg = lift $ lift $ do
    putStrLn msg
    hFlush stdout

getClients :: AppStateIO [(ClientInfo, ProcessHandle)]
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

registerNewClient :: ClientInfo -> ProcessHandle -> AppStateIO ()
registerNewClient clientInfo clientHandle = do
    state <- lift get
    let updated = clients_ state ++ [(clientInfo, clientHandle)]

    lift $ put (AppState updated (cliOptions_ state) (config_ state))

updateClientList :: AppStateIO ()
updateClientList = do
    state <- lift get
    condMap <- mapM
        (\(n, p) -> lift $ lift $ getProcessExitCode p <&> maybe (n, False) (const (n, True)))
            (clients_ state)

    let terminatedClients = map fst $ filter snd condMap
        updated = filter (\(n, _) -> n `notElem` terminatedClients) (clients_ state)

    lift $ put (AppState updated (cliOptions_ state) (config_ state))
