{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE TemplateHaskell #-}

module AppState where

import           CLIOptions.CLIOptions            (CLIOptions (..))
import           CLIOptions.Parser                (parseCLIOptions)
import           Config.Config
import           Config.Loader                    (loadConfig)
import           Minecraft.MinecraftVersion       (MinecraftVersion)
import           Minecraft.Server.ServerBrand     (ServerBrand)

import           Control.Lens                     (makeLenses, over, set)
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
    { _clientProcs :: [(ClientInfo, ProcessHandle)]
    , _serverProc  :: Maybe ProcessHandle
    , _cliOptions  :: CLIOptions
    , _config      :: Config
    }

data ClientInfo = ClientInfo
    { runningClientName    :: String
    , runningClientVersion :: MinecraftVersion
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

absolutePath :: FilePath -> AppStateIO FilePath
absolutePath = lift . lift . makeAbsolute

putStrLn' :: String -> AppStateIO ()
putStrLn' msg = lift $ lift $ do
    putStrLn msg
    hFlush stdout

getClients :: AppStateIO [(ClientInfo, ProcessHandle)]
getClients = lift get <&> _clientProcs

getServerProc :: AppStateIO (Maybe ProcessHandle)
getServerProc = lift get <&> _serverProc

getWorkingDir :: AppStateIO FilePath
getWorkingDir = lift get >>=
    absolutePath . workingDir . applicationConfig . _config

getClientWorkingDir :: AppStateIO FilePath
getClientWorkingDir = getWorkingDir <&> (</> "client")

getBuildDir :: AppStateIO FilePath
getBuildDir = getWorkingDir <&> (</> "build")

getMinecraftDir :: AppStateIO FilePath
getMinecraftDir = lift get >>=
    absolutePath . minecraftDir . _cliOptions

getMinecraftAssetsDir :: AppStateIO FilePath
getMinecraftAssetsDir = getMinecraftDir <&> (</> "assets")

getMinecraftLibrariesDir :: AppStateIO FilePath
getMinecraftLibrariesDir = getMinecraftDir <&> (</> "libraries")

getMinecraftVersionsDir :: AppStateIO FilePath
getMinecraftVersionsDir = getMinecraftDir <&> (</> "versions")

getMinecraftBinDir :: AppStateIO FilePath
getMinecraftBinDir = getMinecraftDir <&> (</> "bin")

getClientDefaultVersion :: AppStateIO MinecraftVersion
getClientDefaultVersion = lift get <&> (clientDefaultVersion . clientConfig . _config)

getClientJvmOptions :: AppStateIO [String]
getClientJvmOptions = lift get <&> (clientJvmOptions . clientConfig . _config)

getServerVersion :: AppStateIO MinecraftVersion
getServerVersion = lift get <&> (serverVersion . serverConfig . _config)

getServerBrand :: AppStateIO ServerBrand
getServerBrand = lift get <&> (serverBrand . serverConfig . _config)

getServerJvmOptions :: AppStateIO [String]
getServerJvmOptions = lift get <&> (serverJvmOptions . serverConfig . _config)

registerNewClient :: ClientInfo -> ProcessHandle -> AppStateIO ()
registerNewClient clientInfo clientHandle = do
    state <- lift get

    lift $ put (over clientProcs (++ [(clientInfo, clientHandle)]) state)

registerServer :: ProcessHandle -> AppStateIO ()
registerServer serverHandle = do
    state <- lift get

    lift $ put (set serverProc (Just serverHandle) state)

updateClientList :: AppStateIO ()
updateClientList = do
    state <- lift get
    condMap <- mapM
        (\(n, p) -> lift $ lift $ getProcessExitCode p <&> maybe (n, False) (const (n, True)))
            (_clientProcs state)

    let terminatedClients = map fst $ filter snd condMap
        clientProcs'      = filter (\(n, _) -> n `notElem` terminatedClients) (_clientProcs state)

    lift $ put (set clientProcs clientProcs' state)

updateServerProc :: AppStateIO ()
updateServerProc = do
    state <- lift get
    case _serverProc state of
        Just sproc ->
            lift (lift (getProcessExitCode sproc)) >>= \case
                Just _  -> lift $ put (set serverProc Nothing state)
                Nothing -> return ()

        Nothing ->
            return ()
