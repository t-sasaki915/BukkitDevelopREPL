module ProcessIO
    ( executeProcess
    , executeProcess'
    , guaranteeSoftwareExistence
    , executeJava
    ) where

import Control.Exception (try)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, throwE)
import GHC.IO.Exception (ExitCode (ExitSuccess))
import System.IO.Error (ioeGetErrorString)
import System.Process (waitForProcess, runProcess)

executeProcess :: String -> [String] -> ExceptT String IO ()
executeProcess exeName args = executeProcess' exeName args Nothing

executeProcess' :: String -> [String] -> Maybe String -> ExceptT String IO ()
executeProcess' exeName args workDir =
    lift (try (runProcess exeName args workDir Nothing Nothing Nothing Nothing)) >>= \case
        Right handler ->
            lift (waitForProcess handler) >>= \case
                ExitSuccess -> return ()
                exitCode    -> throwE (show exitCode)

        Left e ->
            throwE (ioeGetErrorString e)

guaranteeSoftwareExistence :: String -> ExceptT String IO ()
guaranteeSoftwareExistence exeName = executeProcess exeName ["--version"]

executeJava :: String -> [String] -> String -> ExceptT String IO ()
executeJava jarName args workDir =
    executeProcess' "java.exe" (["-jar", jarName] ++ args) (Just workDir)

