module Repl.Command.HelpCommand (HelpCommand(HelpCommand)) where

import           AppState
import           Repl.Command.ReplCommand (ReplCommand (..))
import           Util                     (fillWithSpace)

data HelpCommand = HelpCommand
                 | HelpCommandOptions

instance ReplCommand HelpCommand where
    cmdLabel = const "help"

    cmdDescription = const "Show the command reference of this REPL."

    cmdArgParser = const (pure (pure HelpCommandOptions))

    cmdProcedure = helpCommandProcedure

helpCommandProcedure :: HelpCommand -> AppStateIO ()
helpCommandProcedure _ = do
    putStrLn' "BukkitDevelopREPL Command Reference"
    putStrLn' "For more informations about each command, please refer '<Command> --help'."
    putStrLn' ""

    let maxCmdSize = maximum $ map (length . fst) reference
    mapM_ (\(c, d) -> putStrLn' (fillWithSpace maxCmdSize c ++ " : " ++ d)) reference

reference :: [(String, String)]
reference =
    [ "help"            ~> "Show the command reference of this REPL."
    , "exit"            ~> "Exit the program."
    , "newClient"       ~> "Create a new Minecraft client."
    , "listClient"      ~> "Show a list of Minecraft clients that are currently running."
    , "terminateClient" ~> "Terminate a Minecraft client."
    , "startServer"     ~> "Start the Minecraft server."
    ]
    where (~>) a b = (a, b)
