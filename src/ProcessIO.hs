module ProcessIO
    ( execProcess
    , execProcessQuiet
    , execProcessNewWindow
    , expectExitSuccess
    ) where

import           AppState

import           Control.Exception          (try)
import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Except (throwE)
import           System.Exit                (ExitCode (..))
import           System.IO.Error            (ioeGetErrorString)
import           System.Process

execProcess :: FilePath -> [String] -> FilePath -> String -> AppStateIO ProcessHandle
execProcess execName procArgs procWorkDir errorMsg =
    lift (lift (try (runProcess execName procArgs (Just procWorkDir) Nothing Nothing Nothing Nothing))) >>= \case
        Right handle -> return handle
        Left ioErr   -> throwE (errorMsg ++ ": " ++ ioeGetErrorString ioErr)

execProcessQuiet :: FilePath -> [String] -> FilePath -> String -> AppStateIO ProcessHandle
execProcessQuiet execName procArgs procWorkDir errorMsg =
    lift (lift (try program)) >>= \case
        Right handle -> return handle
        Left ioErr   -> throwE (errorMsg ++ ": " ++ ioeGetErrorString ioErr)
    where
        program = do
            (_, _, _, handle) <-
                createProcess_ "runProcess" (proc execName procArgs)
                    {cwd = Just procWorkDir, std_in = NoStream, std_out = NoStream, std_err = NoStream}
            return handle

execProcessNewWindow :: FilePath -> [String] -> FilePath -> String -> AppStateIO ProcessHandle
execProcessNewWindow execName procArgs procWorkDir errorMsg = do
    lift (lift (try program)) >>= \case
        Right handle -> return handle
        Left ioErr   -> throwE (errorMsg ++ ": " ++ ioeGetErrorString ioErr)
    where
        program = do
            (_, _, _, handle) <-
                createProcess_ "runProcess" (proc execName procArgs)
                    {create_new_console = True, cwd = Just procWorkDir}
            return handle

expectExitSuccess :: String -> ProcessHandle -> AppStateIO ()
expectExitSuccess errorMsg handle =
    lift (lift (waitForProcess handle)) >>= \case
        ExitSuccess     -> return ()
        (ExitFailure n) -> throwE (errorMsg ++ ": ExitCode " ++ show n)
