{-# OPTIONS_GHC -Wno-partial-fields #-}

module Repl.Command.StartServerCommand (StartServerCommand(StartServerCommand)) where

import           AppState
import           FileIO
import           Minecraft.MinecraftPropertyAnalyser   (MCPropertyValue (..),
                                                        lookupProperty,
                                                        parseMinecraftProperty)
import           Minecraft.Server.MinecraftServer      (runMinecraftServer)
import           Minecraft.Server.MinecraftServerSetup (setupMinecraftServer)
import           Minecraft.Server.ServerBrand          (getServerExecutableName)
import           Repl.Command.ReplCommand              (ReplCommand (..))
import           Repl.Util                             (confirmContinue)

import           Control.Monad.Trans.Except            (throwE)
import           Options.Applicative
import           System.FilePath                       ((</>))

data StartServerCommand = StartServerCommand
                        | StartServerCommandOptions
                            { acceptEula :: Bool
                            }

instance ReplCommand StartServerCommand where
    cmdLabel = const "startServer"

    cmdDescription = const "Start the Minecraft server."

    cmdArgParser = const startServerCommandArgParser

    cmdProcedure = startServerCommandProcedure

startServerCommandArgParser :: AppStateIO (Parser StartServerCommand)
startServerCommandArgParser =
    return $
        StartServerCommandOptions
            <$> switch
                ( long "acceptEula"
               <> help "Accept the Minecraft Eula and skip confirming."
                )

startServerCommandProcedure :: StartServerCommand -> AppStateIO ()
startServerCommandProcedure opts = do
    updateServerProc

    getServerProc >>= \case
        Just _ ->
            putStrLn' "The Minecraft server has been started already."

        Nothing -> do
            workingDir    <- getWorkingDir
            serverVersion <- getServerVersion
            serverBrand   <- getServerBrand

            let serverJarPath = workingDir </> getServerExecutableName serverBrand serverVersion

            checkFileExistence serverJarPath
                ("Failed to check the existence of '" ++ serverJarPath ++ "'") >>= \case
                    True -> return ()
                    False -> do
                        putStrLn' ("Could not find '" ++ serverJarPath ++ "'. Need to setup.")

                        setupMinecraftServer

                        putStrLn' "Successfully setup-ed the Minecraft server."

            checkEula (acceptEula opts)

            serverHandle <- runMinecraftServer
            registerServer serverHandle

            putStrLn' "Successfully started the Minecraft server."


checkEula :: Bool -> AppStateIO ()
checkEula skip = do
    workingDir <- getWorkingDir
    let eulaFilePath = workingDir </> "eula.txt"

    let isAccepted = checkFileExistence eulaFilePath
            ("Failed to check the existence of '" ++ eulaFilePath ++ "'") >>= \case
                True -> do
                    eulaFileContent <- readFile' eulaFilePath $
                        "Failed to read a file '" ++ eulaFilePath ++ "'"

                    case parseMinecraftProperty eulaFileContent of
                        Right properties ->
                            case lookupProperty "eula" properties of
                                Just (MCBool True) -> return True
                                _                  -> return False

                        Left err ->
                            throwE ("Failed to parse eula.txt: " ++ err)

                False ->
                    return False

    isAccepted >>= \case
        False | skip ->
            writeFile' eulaFilePath "eula=true" $
                "Failed to write a file '" ++ eulaFilePath ++ "'"

        False -> do
            putStrLn' "To continue, you have to accept the Minecraft Eula:"
            putStrLn' "https://aka.ms/MinecraftEULA"
            putStrLn' ""
            confirmContinue >>= \case
                True ->
                    writeFile' eulaFilePath "eula=true" $
                        "Failed to write a file '" ++ eulaFilePath ++ "'"

                False ->
                    throwE "The operation has cancelled."

        True ->
            return ()
