module Repl.Command.ExitCommand (exitCommandProcedure) where

import           AppState

import           Control.Monad.Trans.Class (lift)
import           System.Exit               (exitSuccess)

exitCommandProcedure :: [String] -> AppStateIO Bool
exitCommandProcedure [] = do
    putStrLn' "Exiting..."
    lift $ lift exitSuccess

exitCommandProcedure _  = return False
