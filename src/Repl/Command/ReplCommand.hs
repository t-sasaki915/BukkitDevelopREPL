module Repl.Command.ReplCommand (ReplCommand(..)) where

import           AppState

import           Options.Applicative (Parser)

class ReplCommand a where
    cmdLabel :: a -> String
    cmdDescription :: a -> String
    cmdArgParser :: a -> AppStateIO (Parser a)
    cmdProcedure :: a -> AppStateIO ()
