{-# OPTIONS_GHC -Wno-partial-fields #-}

module Repl.Command.OpenPluginsDirCommand (OpenPluginsDirCommand(OpenPluginsDirCommand)) where

import           Imports

import           AppState
import           CrossPlatform       (explorerExecName)
import           ProcessIO           (execProcessQuiet)
import           Repl.ReplCommand    (ReplCommand (..))

import           Options.Applicative

data OpenPluginsDirCommand = OpenPluginsDirCommand
                           | OpenPluginsDirCommandOptions
                                { fileOpenerExecName :: String
                                }

instance ReplCommand OpenPluginsDirCommand where
    cmdDescription = const "Open the plugins directory."

    cmdArgParser = const openPluginsDirCommandArgParser

    cmdProcedure = openPluginsDirCommandProcedure

openPluginsDirCommandArgParser :: AppStateIO (Parser OpenPluginsDirCommand)
openPluginsDirCommandArgParser =
    return $
        OpenPluginsDirCommandOptions
            <$> strOption
                ( long "opener"
               <> short 'o'
               <> metavar "FilePath"
               <> value explorerExecName
               <> showDefault
               <> help "Specify the executable of a file opener expressly."
                )

openPluginsDirCommandProcedure :: OpenPluginsDirCommand -> AppStateIO ()
openPluginsDirCommandProcedure opts = do
    workingDir <- getWorkingDir

    let fileOpenerExec = fileOpenerExecName opts
        pluginsDir     = workingDir </> "plugins"

    unlessM (lift (doesDirectoryExist pluginsDir)) $
        lift (createDirectoryIfMissing True pluginsDir)

    void $ execProcessQuiet fileOpenerExec [pluginsDir] workingDir

    putStrLn' "Successfully opened the plugins directory."
