{-# OPTIONS_GHC -Wno-partial-fields #-}

module Repl.Command.StartServerCommand (StartServerCommand(StartServerCommand)) where

import           AppState
import           FileIO
import           Minecraft.Server.MinecraftServer      (runMinecraftServer)
import           Minecraft.Server.MinecraftServerSetup (editServerProperties,
                                                        setupMinecraftServer)
import           Repl.ReplCommand                      (ReplCommand (..),
                                                        confirmContinue)

import           Control.Monad.Trans.Except            (throwE)
import           Data.Minecraft.MCProperty
import           Data.Minecraft.MCServerBrand          (getMCServerExecutableName)
import           Options.Applicative
import           System.FilePath                       ((</>))

data StartServerCommand = StartServerCommand
                        | StartServerCommandOptions
                            { acceptEula :: Bool
                            }

instance ReplCommand StartServerCommand where
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
            serverBrand   <- getMCServerBrand

            let serverJarPath = workingDir </> getMCServerExecutableName serverBrand serverVersion

            checkFileExistence serverJarPath
                ("Failed to check the existence of '" ++ serverJarPath ++ "'") >>= \case
                    True -> return ()
                    False -> do
                        putStrLn' ("Could not find '" ++ serverJarPath ++ "'. Need to download.")

                        setupMinecraftServer

                        putStrLn' "Successfully downloaded the Minecraft server."

            checkEula (acceptEula opts)

            editServerProperties

            serverHandle <- runMinecraftServer
            registerServer serverHandle

            putStrLn' "Successfully started the Minecraft server. The console will be appeared soon."


checkEula :: Bool -> AppStateIO ()
checkEula skip = do
    workingDir <- getWorkingDir
    let eulaFilePath = workingDir </> "eula.txt"

    let accepted = newMCProperties $ addProperty "eula" (MCBool True)
        isAccepted = checkFileExistence eulaFilePath
            ("Failed to check the existence of '" ++ eulaFilePath ++ "'") >>= \case
                True -> do
                    eulaFileContent <- readFile' eulaFilePath $
                        "Failed to read a file '" ++ eulaFilePath ++ "'"

                    case decodeMCProperties eulaFileContent of
                        Right properties ->
                            case mcPropertiesWork (getProperty "eula") properties of
                                Just (MCBool True) -> return True
                                _                  -> return False

                        Left err ->
                            throwE ("Failed to parse eula.txt: " ++ err)

                False ->
                    return False

    isAccepted >>= \case
        False | skip ->
            writeFile' eulaFilePath (encodeMCProperties accepted) $
                "Failed to write a file '" ++ eulaFilePath ++ "'"

        False -> do
            putStrLn' "To continue, you have to accept the Minecraft Eula:"
            putStrLn' "https://aka.ms/MinecraftEULA"
            putStrLn' ""
            confirmContinue >>= \case
                True ->
                    writeFile' eulaFilePath (encodeMCProperties accepted) $
                        "Failed to write a file '" ++ eulaFilePath ++ "'"

                False ->
                    throwE "The operation has cancelled."

        True ->
            return ()
