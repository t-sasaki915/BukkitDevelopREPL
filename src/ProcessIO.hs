module ProcessIO
    ( execProcess
    , expectExitSuccess
    , terminate
    ) where

import AppOptions (AppOptions(..))

import Control.Exception (try)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, throwE)
import Control.Monad.Trans.State.Strict (StateT)
import GHC.IO.Exception (ExitCode (ExitSuccess))
import System.IO.Error (ioeGetErrorString)
import System.Process (ProcessHandle, waitForProcess, runProcess, terminateProcess)

execProcess :: FilePath -> [String] -> String -> String -> ExceptT String (StateT AppOptions IO) ProcessHandle
execProcess execName cmdArgs execDir errorMsg =
    let runner = runProcess execName cmdArgs (Just execDir) Nothing Nothing Nothing Nothing in
        lift (lift (try runner)) >>= \case
            Right handler -> return handler
            Left ioErr    -> throwE (errorMsg ++ ": " ++ ioeGetErrorString ioErr)

expectExitSuccess :: String -> ProcessHandle -> ExceptT String (StateT AppOptions IO) ()
expectExitSuccess errorMsg handler =
    lift (lift (waitForProcess handler)) >>= \case
        ExitSuccess -> return ()
        exitCode    -> throwE (errorMsg ++ ": " ++ show exitCode)

terminate :: ProcessHandle -> ExceptT String (StateT AppOptions IO) ()
terminate handler =
    lift (lift (terminateProcess handler)) >>= const (return ())
