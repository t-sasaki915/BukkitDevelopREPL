module Repl.Command.HelpCommand (HelpCommand(HelpCommand)) where

import           Imports

import           AppState
import           Repl.ReplCommand (ReplCommand (..))

data HelpCommand = HelpCommand
                 | HelpCommandOptions

instance ReplCommand HelpCommand where
    cmdDescription = const "Show the command reference of this REPL."

    cmdArgParser = const (pure (pure HelpCommandOptions))

    cmdProcedure = helpCommandProcedure

helpCommandProcedure :: HelpCommand -> AppStateIO ()
helpCommandProcedure _ = do
    putStrLn' "BukkitDevelopREPL Command Reference"
    putStrLn' "For more informations about each command, please refer '<Command> --help'."
    putStrLn' ""

    let maxCmdSize = maximum $ map (length . fst) reference
        formatter = printf "%%-%ds : %%s" maxCmdSize
    mapM_ (putStrLn' . uncurry (printf formatter)) reference

reference :: [(String, String)]
reference =
    [ "help"            ~> "Show the command reference of this REPL. You can use '?' or 'listCommand' instead."
    , "exit"            ~> "Exit the program. You can use 'quit' or 'stop' instead."
    , "showConfig"      ~> "Show the current configurations."
    , "newClient"       ~> "Create a new Minecraft client."
    , "listClient"      ~> "Show a list of Minecraft clients that are currently running."
    , "terminateClient" ~> "Terminate a Minecraft client."
    , "startServer"     ~> "Start the Minecraft server."
    , "terminateServer" ~> "Terminate the Minecraft server."
    , "restartServer"   ~> "Restart the Minecraft server without confirming."
    , "installPlugins"  ~> "Install plugins to the server."
    ]
    where (~>) a b = (a, b)
