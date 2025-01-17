{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE TemplateHaskell #-}

module AppState where

import           Imports

import           CLIOptions.CLIOptions            (CLIOptions (..))
import           CLIOptions.Parser                (parseCLIOptions)
import           Config.Config
import           Config.Loader                    (loadConfig)

import           Control.Lens                     (makeLenses, over, set)
import           Control.Monad.Trans.State.Strict (StateT, get, put)
import           Data.Maybe                       (fromMaybe)
import           Data.Minecraft.MCGameMode        (MCGameMode)
import           Data.Minecraft.MCServerBrand     (MCServerBrand)
import           Data.Minecraft.MCVersion         (MCVersion)
import           System.Directory                 (makeAbsolute)
import           System.IO                        (hFlush, stdout)
import           System.Process                   (ProcessHandle,
                                                   getProcessExitCode)

type AppStateIO a = HasCallStack => StateT AppState IO a

data AppState = AppState
    { _clientProcs :: [(ClientInfo, ProcessHandle)]
    , _serverProc  :: Maybe ProcessHandle
    , _cliOptions  :: CLIOptions
    , _config      :: Config
    }

data ClientInfo = ClientInfo
    { runningClientName    :: String
    , runningClientVersion :: MCVersion
    }
    deriving Eq

makeLenses ''AppState

initialState :: IO AppState
initialState = do
    cliOpts <- parseCLIOptions
    conf    <- loadConfig (configFile cliOpts)

    let constructor = AppState [] Nothing
    return (constructor cliOpts conf)

getAutoexecCommands :: AppState -> [String]
getAutoexecCommands = autoexecCommands . applicationConfig . _config

getConfigFilePath :: AppStateIO FilePath
getConfigFilePath = get <&> (configFile . _cliOptions)

setConfig :: Config -> AppStateIO ()
setConfig newConfig = do
    state <- get
    put (set config newConfig state)

absolutePath :: FilePath -> AppStateIO FilePath
absolutePath = lift . makeAbsolute

putStrLn' :: String -> AppStateIO ()
putStrLn' msg = lift $ do
    putStrLn msg
    hFlush stdout

getClients :: AppStateIO [(ClientInfo, ProcessHandle)]
getClients = get <&> _clientProcs

getServerProc :: AppStateIO (Maybe ProcessHandle)
getServerProc = get <&> _serverProc

getWorkingDir :: AppStateIO FilePath
getWorkingDir = get >>=
    absolutePath . workingDir . applicationConfig . _config

getDynamicPlugins :: AppStateIO [FilePath]
getDynamicPlugins = get <&> (fromMaybe [] . dynamicPlugins . _cliOptions)

getMinecraftDir :: AppStateIO FilePath
getMinecraftDir = get >>=
    absolutePath . minecraftDir . _cliOptions

getClientDefaultVersion :: AppStateIO MCVersion
getClientDefaultVersion = get <&> (clientDefaultVersion . clientConfig . _config)

getClientJvmOptions :: AppStateIO [String]
getClientJvmOptions = get <&> (clientJvmOptions . clientConfig . _config)

getServerVersion :: AppStateIO MCVersion
getServerVersion = get <&> (serverVersion . serverConfig . _config)

getMCServerBrand :: AppStateIO MCServerBrand
getMCServerBrand = get <&> (serverBrand . serverConfig . _config)

getServerJvmOptions :: AppStateIO [String]
getServerJvmOptions = get <&> (serverJvmOptions . serverConfig . _config)

getStaticPlugins :: AppStateIO [String]
getStaticPlugins = get <&> (serverStaticPlugins . serverConfig . _config)

getServerPort :: AppStateIO Int
getServerPort = get <&> (serverPort . serverConfig . _config)

shouldServerUseOnlineMode :: AppStateIO Bool
shouldServerUseOnlineMode = get <&> (serverOnlineMode . serverConfig . _config)

getServerMotd :: AppStateIO String
getServerMotd = get <&> (serverMotd . serverConfig . _config)

getServerMaxPlayers :: AppStateIO Int
getServerMaxPlayers = get <&> (serverMaxPlayers . serverConfig . _config)

shouldServerEnableCommandBlocks :: AppStateIO Bool
shouldServerEnableCommandBlocks = get <&> (serverEnableCommandBlocks . serverConfig . _config)

getServerDefaultGameMode :: AppStateIO MCGameMode
getServerDefaultGameMode = get <&> (serverDefaultGameMode . serverConfig . _config)

registerNewClient :: ClientInfo -> ProcessHandle -> AppStateIO ()
registerNewClient clientInfo clientHandle = do
    state <- get
    put (over clientProcs (++ [(clientInfo, clientHandle)]) state)

unregisterClient :: String -> AppStateIO ()
unregisterClient clientName = do
    state <- get
    put (over clientProcs (filter (\(ClientInfo cName _, _) -> cName /= clientName)) state)

registerServer :: ProcessHandle -> AppStateIO ()
registerServer serverHandle = do
    state <- get
    put (set serverProc (Just serverHandle) state)

unregisterServer :: AppStateIO ()
unregisterServer = do
    state <- get
    put (set serverProc Nothing state)

updateClientList :: AppStateIO ()
updateClientList = do
    state <- get
    forM_ (_clientProcs state) $ \(ClientInfo cName _, handle) ->
        whenM (lift (getProcessExitCode handle) <&> isJust) $
            unregisterClient cName

updateServerProc :: AppStateIO ()
updateServerProc = do
    state <- get
    whenJust (_serverProc state) $ \sproc ->
        whenM (lift (getProcessExitCode sproc) <&> isJust)
            unregisterServer
