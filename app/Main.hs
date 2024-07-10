module Main (main) where

import Control.Exception (try)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import GHC.IO.Exception (ExitCode (ExitSuccess))
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO.Error (ioeGetErrorString)
import System.Process (waitForProcess, runProcess)

data LauncherError = JavaNotFound String

instance Show LauncherError where
    show (JavaNotFound s) = "The launcher could not find a Java executable: " ++ s

makeWorkingFolder :: ExceptT LauncherError IO ()
makeWorkingFolder = do
    _ <- lift $ createDirectoryIfMissing False ("." </> "run")
    return ()

checkJavaExistence :: ExceptT LauncherError IO ()
checkJavaExistence = do
    process <- lift $ try (runProcess "java" ["-version"] Nothing Nothing Nothing Nothing Nothing)
    case process of
        Right handler ->
            lift (waitForProcess handler) >>= \case
                ExitSuccess -> return ()
                exitCode    -> throwE (JavaNotFound (show exitCode))

        Left e ->
            throwE (JavaNotFound (ioeGetErrorString e))

program :: ExceptT LauncherError IO ()
program = do
    makeWorkingFolder
    checkJavaExistence

main :: IO ()
main = runExceptT program >>= either print (const (return ()))
