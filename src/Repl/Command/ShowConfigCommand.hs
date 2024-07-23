module Repl.Command.ShowConfigCommand (ShowConfigCommand(ShowConfigCommand)) where

import           AppState
import           Repl.ReplCommand                 (ReplCommand (..))

import           Control.Monad.Trans.Class        (lift)
import           Control.Monad.Trans.State.Strict (get)
import           Data.ByteString                  (unpack)
import           Data.ByteString.Internal         (w2c)
import           Data.Functor                     ((<&>))
import           Data.Yaml                        (encode)

data ShowConfigCommand = ShowConfigCommand
                       | ShowConfigCommandOptions

instance ReplCommand ShowConfigCommand where
    cmdLabel = const "showConfig"

    cmdDescription = const "Show the current configurations."

    cmdArgParser = const (pure (pure ShowConfigCommandOptions))

    cmdProcedure = showConfigCommandProcedure

showConfigCommandProcedure :: ShowConfigCommand -> AppStateIO ()
showConfigCommandProcedure _ = do
    currentConfig <- lift get <&> _config

    let encoded = encode currentConfig
    putStrLn' (map w2c (unpack encoded))
