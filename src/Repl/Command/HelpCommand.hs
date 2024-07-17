module Repl.Command.HelpCommand (HelpCommand(HelpCommand)) where

import           AppState
import           Repl.Command.ReplCommand (ReplCommand (..))

data HelpCommand = HelpCommand
                 | HelpCommandOptions

instance ReplCommand HelpCommand where
    cmdLabel = const "help"

    cmdDescription = const "Show the command reference of this REPL."

    cmdArgParser = const (pure (pure HelpCommandOptions))

    cmdProcedure = helpCommandProcedure

helpCommandProcedure :: HelpCommand -> AppStateIO ()
helpCommandProcedure _ =
    putStrLn' $ unlines
        [ "spigot-debugger-launcher REPL Command Reference"
        , "For more informations about each command, please refer '<Command> --help'."
        , ""
        , "  help            : Show the command reference of this REPL."
        , "  exit            : Exit the program."
        , "  newClient       : Create a new Minecraft client."
        , "  listClient      : Show a list of Minecraft clients that are currently running."
        , "  terminateClient : Terminate a Minecraft client."
        ]
