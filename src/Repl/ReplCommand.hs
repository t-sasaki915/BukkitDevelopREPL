module Repl.ReplCommand (ReplCommand(..), confirmContinue) where

import           AppState

import           Control.Monad.Trans.Class (lift)
import           Data.Char                 (toLower)
import           Options.Applicative       (Parser)
import           System.IO                 (hFlush, stdout)
import           Text.Regex.Posix          ((=~))

class ReplCommand a where
    cmdDescription :: a -> String
    cmdArgParser :: a -> AppStateIO (Parser a)
    cmdProcedure :: a -> AppStateIO ()

confirmContinue :: AppStateIO Bool
confirmContinue = do
    input <- lift $ lift $ do
        putStr "Continue? (Y/N): "
        hFlush stdout
        getLine

    let lowerCase = map toLower input
    if lowerCase =~ ("^(y(es)?|aye)$" :: String) then
        return True
    else if lowerCase =~ ("^no?$" :: String) then
        return False
    else
        confirmContinue

