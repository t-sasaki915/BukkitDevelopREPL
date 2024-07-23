module CLIOptions.Parser (parseCLIOptions) where

import           Imports

import           CLIOptions.CLIOptions (CLIOptions, cliOptionsParser)

import           Data.Version          (showVersion)
import           Options.Applicative

parseCLIOptions :: IO CLIOptions
parseCLIOptions = do
    parser <- cliOptionsParser
    customExecParser
        (prefs disambiguate)
            (info (helper <*> parser)
                (fullDesc <> header
                    (printf "BukkitDevelopREPL %s by TSasaki" (showVersion version))))

