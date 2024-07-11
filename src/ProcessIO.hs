module ProcessIO
    ( execProcess
    , expectExitSuccess
    ) where

import AppOptions (AppOptions(..))

import Control.Exception (try)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, throwE)
import Control.Monad.Trans.State.Strict (StateT)
import GHC.IO.Exception (ExitCode (ExitSuccess))
import System.IO.Error (ioeGetErrorString)
import System.Process (ProcessHandle, waitForProcess, runProcess)

execProcess :: FilePath -> [String] -> String -> ExceptT String (StateT AppOptions IO) ProcessHandle
execProcess execName cmdArgs execDir =
    let runner = runProcess execName cmdArgs (Just execDir) Nothing Nothing Nothing Nothing in
        lift (lift (try runner)) >>= \case
            Right handler -> return handler
            Left ioErr    -> throwE $
                "Failed to execute an external process: " ++ ioeGetErrorString ioErr

expectExitSuccess :: ProcessHandle -> ExceptT String (StateT AppOptions IO) ()
expectExitSuccess handler =
    lift (lift (waitForProcess handler)) >>= \case
        ExitSuccess -> return ()
        exitCode    -> throwE ("An external process has terminated: " ++ show exitCode)
