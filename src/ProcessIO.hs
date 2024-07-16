module ProcessIO (execProcess, expectExitSuccess) where

import           AppState

import           Control.Exception          (try)
import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Except (throwE)
import           System.Exit                (ExitCode (..))
import           System.IO.Error            (ioeGetErrorString)
import           System.Process             (ProcessHandle, runProcess,
                                             waitForProcess)

execProcess :: FilePath -> [String] -> FilePath -> String -> AppStateIO ProcessHandle
execProcess execName procArgs procWorkDir errorMsg =
    lift (lift (try (runProcess execName procArgs (Just procWorkDir) Nothing Nothing Nothing Nothing))) >>= \case
        Right handle -> return handle
        Left ioErr   -> throwE (errorMsg ++ ": " ++ ioeGetErrorString ioErr)

expectExitSuccess :: String -> ProcessHandle -> AppStateIO ()
expectExitSuccess errorMsg handle =
    lift (lift (waitForProcess handle)) >>= \case
        ExitSuccess     -> return ()
        (ExitFailure n) -> throwE (errorMsg ++ ": ExitCode " ++ show n)
