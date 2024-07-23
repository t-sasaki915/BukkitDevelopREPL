{-# OPTIONS_GHC -Wno-partial-fields #-}

module Repl.Command.InstallPluginsCommand (InstallPluginsCommand(InstallPluginsCommand)) where

import           Imports

import           AppState
import           Minecraft.Server.PluginInstaller
import           Repl.Command.StartServerCommand     (StartServerCommand (StartServerCommand))
import           Repl.Command.TerminateServerCommand (TerminateServerCommand (TerminateServerCommand))
import           Repl.ReplCommand                    (ReplCommand (..))

import           Options.Applicative

data InstallPluginsCommand = InstallPluginsCommand
                           | InstallPluginsCommandOptions
                                { dynamicPluginsOnly :: Bool
                                , staticPluginsOnly  :: Bool
                                , andRestart         :: Bool
                                , withoutAsk         :: Bool
                                }

instance ReplCommand InstallPluginsCommand where
    cmdDescription = const "Install plugins to the server."

    cmdArgParser = const installPluginsCommandArgParser

    cmdProcedure = installPluginsCommandProcedure

installPluginsCommandArgParser :: AppStateIO (Parser InstallPluginsCommand)
installPluginsCommandArgParser =
    return $
        InstallPluginsCommandOptions
            <$> switch
                ( long "dynamicOnly"
               <> short 'd'
               <> help "Install only dynamic plugins."
                )
            <*> switch
                ( long "staticOnly"
               <> short 's'
               <> help "Install only static plugins."
                )
            <*> switch
                ( long "restart"
               <> short 'r'
               <> help "Restart the Minecraft server after installation."
                )
            <*> switch
                ( long "force"
               <> short 'f'
               <> help "Restart the Minecraft server without confirming."
                )

installPluginsCommandProcedure :: InstallPluginsCommand -> AppStateIO ()
installPluginsCommandProcedure opts = do
    let dynamicOnly = dynamicPluginsOnly opts
        staticOnly  = staticPluginsOnly opts
        restart     = andRestart opts
        force       = withoutAsk opts

    when (dynamicOnly && staticOnly) $
        throwE "Please do not specify both '--dynamicOnly' and '--staticOnly' at the same time."

    updateServerProc

    whenM (getServerProc <&> isJust) $ do
        unless restart $
            throwE "The Minecraft server is running. Please stop it first. Or you can use '--restart' option."

        executeReplCommandInternal TerminateServerCommand ["--force" | force]

    initialisePluginFileNameMap
    removeUnusedPlugins

    dynamicPlugins <- getDynamicPlugins
    staticPlugins  <- getStaticPlugins

    unless staticOnly $
        if null dynamicPlugins then
            putStrLn' "There was no dynamic plugin to install."

        else do
            installDynamicPlugins
            putStrLn' "Successfully installed dynamic plugins."

    unless dynamicOnly $ do
        if null staticPlugins then
            putStrLn' "There was no static plugin to install."

        else do
            installStaticPlugins
            putStrLn' "Successfully installed static plugins."

    when restart $
        executeReplCommandInternal StartServerCommand []
