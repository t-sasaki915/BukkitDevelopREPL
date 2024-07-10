module ClientLauncher (ClientError(..), launchClient) where

import Constant
import ProcessIO (executeJava)

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, withExceptT)
import System.Directory (getHomeDirectory)

newtype ClientError = ClientLaunchError String

instance Show ClientError where
    show (ClientLaunchError e) =
        "The launcher could not launch a client: " ++ e

launchClient :: ExceptT ClientError IO ()
launchClient = do
    homeDir <- lift getHomeDirectory
    withExceptT ClientLaunchError $
        executeJava (minecraftClientJVMOptions ++ minecraftClientOptions homeDir)
            workingDirPath
