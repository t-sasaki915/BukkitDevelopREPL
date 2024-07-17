module Main (main) where

import           AppState
import           FileIO                              (makeDirectory)
import           Minecraft.Client.ClientJsonAnalyser
import           Repl.Repl                           (startRepl)

import           Control.Monad.Trans.Except          (runExceptT)
import           Control.Monad.Trans.State.Strict    (runStateT)
import           System.Exit                         (exitFailure, exitSuccess)

makeNecessaryDirectories :: AppStateIO ()
makeNecessaryDirectories = do
    workingDir <- getWorkingDir

    makeDirectory workingDir $
        "Failed to make a directory '" ++ workingDir ++ "'"

program :: AppStateIO ()
program = do
    v <- getClientDefaultVersion
    assetIndex <- getAssetIndex v
    mainClass  <- getMainClass v
    libraries  <- getLibraries v
    putStrLn' assetIndex
    putStrLn' mainClass
    putStrLn' (show libraries)
    makeNecessaryDirectories
    startRepl

main :: IO ()
main = do
    initState   <- initialState
    (result, _) <- runStateT (runExceptT program) initState
    case result of
        Right () -> exitSuccess
        Left err -> putStrLn err >> exitFailure
