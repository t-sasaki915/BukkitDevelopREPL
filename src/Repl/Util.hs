module Repl.Util (confirmContinue) where

import           AppState

import           Control.Monad.Trans.Class (lift)
import           Data.Char                 (toLower)
import           System.IO                 (hFlush, stdout)
import           Text.Regex.Posix          ((=~))

confirmContinue :: AppStateIO Bool
confirmContinue = do
    input <- lift $ lift $ do
        putStr "Continue? (Y/N): "
        hFlush stdout
        getLine

    let lowerCase = map toLower input
    if lowerCase =~ "^(y(es)?|aye)$" then
        return True
    else if lowerCase =~ "^no?$" then
        return False
    else
        confirmContinue
