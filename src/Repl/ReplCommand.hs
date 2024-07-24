module Repl.ReplCommand (ReplCommand(..), confirmContinue) where

import           Imports

import           AppState

import           Data.Char           (toLower)
import           Options.Applicative
import           System.IO           (hFlush, stdout)

class ReplCommand a where
    cmdDescription :: a -> String
    cmdArgParser   :: a -> AppStateIO (Parser a)
    cmdProcedure   :: a -> AppStateIO ()

    executeReplCommand :: a -> String -> [String] -> AppStateIO ()
    executeReplCommand cmd label args = do
        parser <- cmdArgParser cmd
        let executor =
                execParserPure
                    (prefs disambiguate)
                        (info (helper <*> parser)
                            (fullDesc <> progDesc (cmdDescription cmd)))
        case executor args of
            Success parsedArgs ->
                cmdProcedure parsedArgs

            Failure err ->
                let (helpMsg, _, _) = execFailure err label in
                    putStrLn' (show helpMsg)

            CompletionInvoked _ ->
                return ()

    executeReplCommandInternal :: a -> [String] -> AppStateIO ()
    executeReplCommandInternal cmd = executeReplCommand cmd "INTERNAL"

confirmContinue :: AppStateIO Bool
confirmContinue = do
    input <- lift $ do
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

