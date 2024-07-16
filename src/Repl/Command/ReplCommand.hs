module Repl.Command.ReplCommand (Command(..), ReplCommand(..), replCommands) where

import           AppState
import           Repl.Command.ExitCommand (exitCommandProcedure)

class Command a where
    cmdLabel       :: a -> String
    cmdDescription :: a -> String
    cmdUsage       :: a -> String
    cmdProcedure   :: a -> [String] -> AppStateIO Bool

data ReplCommand = ExitCommand

replCommands :: [ReplCommand]
replCommands =
    [ ExitCommand
    ]

instance Command ReplCommand where
    cmdLabel       ExitCommand = "exit"

    cmdDescription ExitCommand = "Exit the REPL."

    cmdUsage       ExitCommand = ""

    cmdProcedure   ExitCommand = exitCommandProcedure
