module ProcessIO
    ( execProcess
    , execProcessQuiet
    , execProcessAndGetOutput
    , expectExitSuccess
    ) where

import           Imports

import           AppState

import           System.Exit    (ExitCode (..))
import           System.Process

execProcess :: FilePath -> [String] -> FilePath -> AppStateIO ProcessHandle
execProcess execName procArgs procWorkDir = lift $ do
    (_, _, _, handle) <-
        createProcess (proc execName procArgs)
            { cwd = Just procWorkDir
            }
    return handle

execProcessQuiet :: FilePath -> [String] -> FilePath -> AppStateIO ProcessHandle
execProcessQuiet execName procArgs procWorkDir = lift $ do
    (_, _, _, handle) <-
        createProcess (proc execName procArgs)
            { cwd = Just procWorkDir
            , std_in = NoStream
            , std_out = NoStream
            , std_err = NoStream
            }
    return handle

execProcessAndGetOutput :: FilePath -> [String] -> FilePath -> AppStateIO String
execProcessAndGetOutput execName procArgs procWorkDir = lift $
    readCreateProcess (proc execName procArgs)
        { cwd = Just procWorkDir
        }
        []

expectExitSuccess :: String -> ProcessHandle -> AppStateIO ()
expectExitSuccess errorMsg handle =
    lift (waitForProcess handle) >>= \case
        ExitSuccess     -> return ()
        (ExitFailure n) -> error (printf errorMsg (printf "ExitCode %d" n :: String))
